# G. Smith-Vidaurre
# R translation of Eric Fossler-Lussier's original Python energy detector
# 07 September 2020


energyDetector <- function(wavs, path, outpath, lowerfreq, higherfreq, energyoffset, amplitudethreshold, spec_fft_size, spec_fft_shift, spec_energy_floor, truncate, shouldplot, strategy, boundarystrategy){
  
  ######################### Define helper functions ###################################
  
  # Energy masking function
  # Purpose: After computing the log of the sum of the spectral density at each timestep (e.g over each periodogram), perform energy masking by asking which energies are greater than a given threshold. Energies above the given energy threshold will be considered "signal", all the rest will be noise
  compute_energy_mask <- function(spec, freqs, lowerfreq, higherfreq, energyoffset){
    
    # Calculate the energy index of the lower and upper bounds
    fix_low <- which(freqs == min(freqs[freqs > lowerfreq]))
    fix_high <- which(freqs == min(freqs[freqs > higherfreq])) 
    
    # Get the log of the summed spectral energies per timestep
    # In other words, per timestep, sum the spectral energies over the given indices, then take the log
    v <- sapply(1:length(spec), function(i){
      log(sum(spec[[i]][fix_low:fix_high]))
    })
    
    # Calculate the energy threshold for masking from the median energy 
    threshold <- median(v) + energyoffset
    
    # Perform the energy masking, return the Boolean mask
    mask <- v > threshold
    
    # Return the Boolean mask and the log summed spectral densities   
    return(list(spec_mask = mask, log_sum_energy = v))
  }
  
  # Lowpass Butterworth filter function
  # Purpose: Implement a Butterworth bandpass filter to lowpass a given amplitude envelope. This function is called by compute_amplitude_mask()
  lowpass <- function(signal, fs, cutoff){
    # Initialize the filter itself
    bf <- signal::butter(n = 1, W = cutoff / (fs / 2), type = "low")
    # Apply the filter to the given signal
    filt_signal <- signal::filter(bf, signal)
    return(filt_signal)
  }
  
  # Detection merging function
  # Purpose: Remove gaps between segments detected as signal, merge into larger detections
  
  
  # mask = amp_mask_res$amp_mask
  # freq = freq
  # remove_segment_gaps(amplitudemask, freq)
  
  # mask = detection 
  # freq = freq
  
  remove_segment_gaps <- function(mask, freq){
    
    # Mask is already Boolean
    detection <- mask
      
    detectionchange <- c(0, diff(as.integer(detection)))
      
    lookaheadwindow <- 0.001*freq
    onsets <- which(detectionchange > 0)
    offsets <- which(detectionchange < 0)
    
    # Only remove gaps if there is more than one detection
    if(length(offsets) > 1){
      
      for(i in 1:(length(offsets) - 1)){
        # If the difference between the given offset and the next onset is too short, remove offset
        if((onsets[i + 1] - offsets[i]) < lookaheadwindow){
          detection[offsets[i]:onsets[i + 1]] <- TRUE
        }
      }
    } 
    
    return(detection)
    
  }
  
  # Amplitude masking function
  # Purpose: After smoothing the amplitude envelope with a lowpass Butterworth filter, mask the amplitude envelope by a previously initialized amplitude threshold. Amplitudes above the given threshold will be considered "signal", all the rest will be noise
  
  # wav = wav
  # freq = freq
  # amplitudethreshold = amplitudethreshold
  # amplitudethreshold = 1000
 
  compute_amplitude_mask <- function(wav, freq, amplitudethreshold){
    # Get local envelope, smooth with low-pass filter
    envelope <- seewave::env(wav, f = wav@samp.rate, envt = "hil", plot = FALSE)

    smoothedenvelope <- lowpass(envelope, freq, freq/50)
    
    # Perform masking on just the amplitude envelope, Boolean result
    detection <- (smoothedenvelope > amplitudethreshold)
    # unique(detection)
    # which(detection)
    
    # If this fails, just take the detection object, can add a tryCatch as needed
    detection2 <- remove_segment_gaps(mask = detection, freq = freq)
    
    return(list(amp_mask = detection2, smoothedenvelope = smoothedenvelope))
  }
  
  # Convert mask to segment
  # Purpose: Return onset and offset indices of segments, given a frequency or amplitude mask
  mask2segment <- function(mask){
    change <- c(0, diff(as.integer(mask)))
    onsets <- which(change > 0)
    offsets <- which(change < 0)
    
    # 139 offsets and onsets. At this point in his script, Eric has only 15
    if(length(onsets) > length(offsets)){
      onsets <- onsets[0:length(offsets)]
    }
    
    if(length(offsets) > length(onsets)){
      offsets <- offsets[length(offsets) - length(onsets):length(offsets)]
    }
    
    return(data.frame(onsets, offsets))
    
  }
  
  ######################################### TESTING ####################################################

  # Align segments 
  # Purpose: Align detected signal segments between the energy and amplitude streams, for the hybrid mask resolving below
  # energymask = energy_mask_res$spec_mask
  # amplitudemask = amp_mask_res$amp_mask
  # energytime = t
  # amplitudetime = tm
  # freq = freq
  # 
  # unique(energymask)
  # unique(amplitudemask)
  
  align_segments <- function(energymask, amplitudemask, energytime, amplitudetime, freq){
    
    amplitudemask2 <- remove_segment_gaps(amplitudemask, freq)
    amplitudesegments <- mask2segment(amplitudemask2)
    
    # energysegments doesn't exist yet
    # if(nrow(energysegments) > 0){
      
    # Upsample energysegments to amplitudetime
    energysegments <- mask2segment(energymask)
    upsampleenergymask <- rep(0, length(amplitudemask2))
    
    if (nrow(energysegments) > 0 & all(complete.cases(energysegments))) {
      # i <- 1
      for(i in 1:nrow(energysegments)){
        segstart <- as.integer(energytime[energysegments[i, 1]]*freq)
        segend <- as.integer(energytime[energysegments[i, 2]]*freq)
        upsampleenergymask[segstart:segend] <- 1
      }
      
      energysegments <- mask2segment(upsampleenergymask)
    }

    
    # Algorithm: find regions where both measures make a detection
    # Fill in any gaps between detected regions < 1ms
    # Run this only if both energysegments and amplitudesegments actually contain detections
    
    if(nrow(energysegments) > 0 & all(complete.cases(energysegments)) & nrow(amplitudesegments) > 0 & all(complete.cases(amplitudesegments))){
      
      
      # ################################ TESTING ###########################################
      # 
      # i <- 1
      # j <- 1
      emask <- as.logical(rep(1, nrow(energysegments)))
      amask <- as.logical(rep(1, nrow(amplitudesegments)))
      # 
      # while (i < nrow(energysegments) & j < nrow(amplitudesegments)){
      #   
      #   # cat(paste("i = ", i, "j = ", j, "\n"))
      #   
      #   # Detect whether centroid falls in each range
      #   # Julia and I corrected calculation of the midpoint
      #   ecentroid <- ((energysegments[i, 2] - energysegments[i, 1])/2) + energysegments[i, 1]
      #   if(ecentroid < amplitudesegments[j, 1]){
      #     emask[i] <- FALSE
      #   }
      #   
      #   # i <- i + 1
      #   
      #   acentroid <- ((amplitudesegments[j, 2] - amplitudesegments[j, 1])/2) + amplitudesegments[j, 1]
      #   if(acentroid < energysegments[i, 1]){
      #     amask[j] <- FALSE
      #   }
      #   
      #   i <- i + 1
      #   j <- j + 1
      #   
      #   # Eric's note: 
      #   # CHECK THIS: there may be weird cases where the above doesn't catch something mutually overlapping, continue
      #   # i <- i + 1
      #   # j <- j + 1
      #   
      #   if(i < nrow(energysegments)){
      #     emask[i:length(emask)] <- FALSE
      #   }
      #   if(j < nrow(amplitudesegments)){
      #     amask[j:length(amask)] <- FALSE
      #   }
      #   
      #   # energysegments <- energysegments[emask:length(energysegments)] #? or below
      #   energysegments <- energysegments[emask, ]
      #   # amplitudesegments <- amplitudesegments[amask:length(amplitudesegments)] # ? or below
      #   amplitudesegments <- amplitudesegments[amask, ]
      #   
      # }
      
      # Try a different approach from the while loop to check whether there are mutually overlapping detections
      # These objects contain indices
      energysegment_overlaps <- which(energysegments$onsets >= min(amplitudesegments$onsets) & energysegments$offsets <= max(amplitudesegments$offsets))
      
      amplitudesegment_overlaps <- which(amplitudesegments$onsets >= min(energysegments$onsets) & amplitudesegments$offsets <= max(energysegments$offsets))
      
      # Create masks using indices of the overlapping sections
      emask[-energysegment_overlaps] <- FALSE
      # emask # Indices not found to overlap with amplitude detections are set to FALSE
      
      amask[-amplitudesegment_overlaps] <- FALSE
      # amask # Indices not found to overlap with energy detections are set to FALSE
      
      # Apply the masks to the energysegments and amplitudesegments objects
      # This will return only indices with values of TRUE, e.g. only overlapping detections will be retained
      energysegments <- energysegments[emask, ]
      amplitudesegments <- amplitudesegments[amask, ]
      
      
      ################################################################################################
      
      # Check whether the energy and amplitude segments still have data after masking
      if(nrow(energysegments) == 0){
        energysegments <- data.frame(onsets = NA, offsets = NA)
      }
      
      if(nrow(amplitudesegments) == 0){
        amplitudesegments <- data.frame(onsets = NA, offsets = NA)
      }
      
    } else if(nrow(energysegments) == 0 & nrow(amplitudesegments) > 0){
      
      amplitudesegments <- amplitudesegments
      energysegments <- data.frame(onsets = NA, offsets = NA)
      
    } else if(nrow(energysegments) > 0 & nrow(amplitudesegments) == 0){
      
      energysegments <- energysegments
      amplitudesegments <- data.frame(onsets = NA, offsets = NA)
      
    }
    
    return(list(energysegments = energysegments, amplitudesegments = amplitudesegments))
    
  }
  
  ######################################################################################################
  
  # Convert segment to mask
  # Purpose: Return a mask, given segments as input
  # segments = amplitude_segments
  # lngth = length(amplitudemask)
  
  segment2mask <- function(segments, lngth){
    mask <- rep(0, lngth)
    # Only perform masking if there are actually segments
    if(!all(is.na(segments$onsets)) & !all(is.na(segments$offsets))){
      for(i in 1:nrow(segments)){
        mask[segments[i, 1]:segments[i, 2]] <- 1
      }
    }
    return(mask)
  }
  
  # Mask resolving function
  # Purpose: Resolve discrepancies in detections. This function allows you to pick which masking strategy should be trusted as the primary mask that yields detection segments, and which strategy should be trusted for the final placement of detection boundaries (e.g. temporal coordinates). Here, the options are either frequency masks, amplitude masks, or hybrid (combined masks)
  # Eric's documentation:
  #   strategy - which mask is taken as primary
  #       frequency: any segment found in frequency stream is output
  #       amplitude: any segment found in amplitude stream is output
  #       hybrid: any segment found in both streams is output (default)
  #   boundarystrategy - which mask determines placement of boundary
  #       frequency: trust frequency-derived boundaries
  #       amplitude: trust amplitude-derived boundaries
  #       hybrid: trust amplitude for onset, frequency for offset
  #       same: use setting from strategy (default)
  
  # frequencymask = energy_mask_res$spec_mask
  # amplitudemask = amp_mask_res$amp_mask
  # t = t
  # tm = tm
  # strategy = strategy
  # boundarystrategy = boundarystrategy
  # # boundarystrategy = "frequency"
  # freq = freq

  # previously, freq = 250000
  resolve_mask <- function(frequencymask, amplitudemask, t, tm, strategy, boundarystrategy, freq = freq){
    
    if(strategy == "frequency"){
      if(boundarystrategy == "frequency" | boundarystrategy == "same"){
        return(list(combined_mask = frequencymask, combined_segs = mask2segment(frequencymask), t = t))
      } else{
        stop('frequency strategy only works with boundarystrategy of frequency or same')
      }
    } else if(strategy == "amplitude"){
      if(boundarystrategy == "amplitude" | boundarystrategy == "same"){
        return(list(combined_mask = amplitudemask, combined_segs = mask2segment(amplitudemask), tm = tm))
      } else {
        stop('amplitude strategy only works with boundarystrategy of amplitude or same')
      }
      # Hybrid strategy
      # Returns only segments in both 
    } else {
      align_res <- align_segments(frequencymask, amplitudemask, t, tm, freq)
      frequency_segments <- align_res$energysegments
      amplitude_segments <- align_res$amplitudesegments
      if (boundarystrategy == "frequency"){
        return(list(combined_mask = segment2mask(frequency_segments, length(frequencymask)), combined_segs = frequency_segments))
      } else if(boundarystrategy == "amplitude"){
        return(list(combined_mask = segment2mask(amplitude_segments, length(amplitudemask)), combined_segs = amplitude_segments))
      } else {
        # I think equivalent is t(c(vector1, vector2))
        # combined_segments=np.transpose(np.vstack((amplitude_segments[:,0],frequency_segments[:,1])))
        # Combine the amplitude and hybrid streams by taking the amplitude start indices and the frequency end indices
         combined_segments <- data.frame(onset = amplitude_segments[, 1], offset = frequency_segments[, 2])
        return(list(combined_mask = segment2mask(combined_segments, length(amplitudemask)), combined_segs = combined_segments))
      }
    }
  }
  
  # Clipping scoring function
  # Purpose: Score each detection by whether or not it's clipped (score between 0 and 1). If 10 or more samples are above 0.99 then the signal will be considered clipped
  # Samples are scaled from [-1, 1] wen .wav object is read into R
  
  # z <- 2
  # samples <- dats[detections[z, "onsets"]:detections[z, "offsets"]]
  
  get_clip_score <- function(samples){
    score <- sum(abs(samples) > 0.99)/10
    return(ifelse(score > 0, "Y", "N"))
  }
  
  # Autocorrelation function
  # Purpose: Autocorrelation function used when estimating pitch (fundamental frequency)
  autocorr <- function(x){
    # Note that the values in tmp are many orders of magnitude greater than those that Eric's code returns, seems due to differences in cross-correlation implementation between R and Python
    tmp <- ccf(x, x, lag.max = length(x), plot = FALSE, type = "correlation")
    return(tmp$acf[ceiling((length(tmp$acf)/2)):length(tmp$acf)])
  }
  
  # Pitch estimator function. Is this still necessary if not making visuals?
  # Purpose: Estimate pitch (fundamental frequency) along the given detection
  pitch <- function(x, freqix, minfreq = 15000){
    cc <- autocorr(x)
    cc[freqix < minfreq] <- 0
    maxix <- which(cc == max(cc))
    p <- freqix[maxix]
    
    # Check for pitch doubling/tripling, hacky bit of code
    if((cc[floor(maxix/2)] / cc[maxix]) > 0.2){
      p <- freqix[floor(maxix/2)]
    } else if((cc[floor(maxix/3)] / cc[maxix]) > 0.2){
      p <- freqix[floor(maxix/3)]
    }
    
    return(p)
  }
  
  # Open connection to error log file
  WavError <- file.path(path, paste(paste("energyDetectorLog", Sys.Date(), sep = "_"), ".txt", sep = ""))
  
  invisible(pblapply(1:length(wavs), function(w){
    
    # cat(paste("w = ", w, "\n"))
    
    # path <- "E:/Gerry_Grace_bat_calls/Data/detection_expt/Roch_GMM_detector"
    # wavs <- list.files(path, pattern = ".WAV$")
    # w <- 11
    
    # Read in Wave data in 16-bit format; return error if not a Wave
    obj <- try(readWave(file.path(path, wavs[w])), silent = TRUE)
    if(!is(obj,"try-error")) {
      # By default, Eric's Python code relies on sf.read from soundfile package to read in the .wav data as Float64: https://pysoundfile.readthedocs.io/en/latest/
      # Perform rescaling with tuneR, note that no normalization (rescale) was performed, to match Eric's code
      # The resulting data is the closest to Eric's results that I've gotten, and are the right order of magnitude
      # I think something is happening here with amplitude values...is this the right order of magnitude? Also "unit64" should scale from [-1, 1]...yep, added the argument pcm = FALSE and this looks better 
      wav <- tuneR::normalize(obj, unit = "64", rescale = FALSE, pcm = FALSE)
      # str(wav)
      # head(wav@left)
      
      # Get amplitude values (e.g. the sampled data)
      dats <- wav@left
      freq <- wav@samp.rate
      
      # Compute signal length in seconds
      xmin <- 0
      
      xmax <- length(dats)/freq
      # xmax
      
      # Remove signal mean to zero-center (e.g. remove DC offset)
      dats <- (dats - mean(dats))
      # head(dats)
      
      # Truncate data to region of interest
      if(truncate){
        dats <- dats[as.integer(freq * xmin):as.integer(freq * xmax) + 1]
      }
      # head(dats)
      
      # Get the two dimensional spectral density with periodograms per timestep, corresponds to the object spec that Eric creates
      # The dimensions of the following object are correct. The numbers within are several orders of magnitude greater than what Eric's function returns, but the closest I've gotten so far
      
      # For the example file I was working with, 2019-07-08_toast_T0001436.WAV: 
      # 256 frequency bands, contained in @freq, a vector of frequencies at which spectral density is estimated
      # 105217 amplitude/energy values per band, contained in @spec. The 105217 samples were generated by a sliding window over the signal with overlap  
      # List of vectors or matrices of the spec values returned by spec.pgram at frequencies corresponding to freq. Each element of the list corresponds to one periodogram estimated from samples of the window beginning at start of the Wave or WaveMC object.
      spec_dens <- tuneR::periodogram(wav, width = spec_fft_size, overlap = spec_fft_size - spec_fft_shift, normalize = FALSE, downsample = 250000, channel = "left", units = "samples", taper = 0)
      # str(spec_dens)
      
      # In Python, the spec object is made by calling the function specgram: https://matplotlib.org/api/_as_gen/matplotlib.pyplot.specgram.html. Just saw that Eric did this with sampling rate 250000, but downsampling makes no difference
      spec <- spec_dens@spec # These numbers are different than Eric's object, which are e-12 or e-3
      freqs <- spec_dens@freq
      # str(spec)
      
      # Get the energy mask in order to test subsequent translated functions
      energy_mask_res <- compute_energy_mask(spec = spec, freqs = freq, lowerfreq = lowerfreq, higherfreq = higherfreq, energyoffset = energyoffset)  
      # str(energy_mask_res)
      
      # Get the amplitude mask results to keep testing
      amp_mask_res <- compute_amplitude_mask(wav = wav, freq = freq, amplitudethreshold = amplitudethreshold)
      # str(amp_mask_res)
      
      # Sample temporal coordinates
      # Purpose: Compute times per each sample in the data (e.g. each data point, not each detection). Here these look exactly like those returned by Eric's code
      tm <- xmin + seq(1, length(dats), 1)/freq
      # length(tm)
      
      # Temporal midpoints of periodogram windows
      # The times corresponding to midpoints of segments (i.e., the columns in spectrum), returned by Python specgram
      mids <- (spec_dens@starts + unique(diff(spec_dens@starts)/2)) # Indices
      t <- tm[mids] # A bit off from Eric's numbers
      # length(t)
      # length(tm)
      
      # Purpose? Resolve discrepancies in detections. The detections are in the object called combined segs, in which each row is a detection, onsets = starts, and offsets = ends
      combined_res <- resolve_mask(frequencymask = energy_mask_res$spec_mask, amplitudemask = amp_mask_res$amp_mask, t = t, tm = tm, strategy = strategy, boundarystrategy = boundarystrategy, freq = freq)
      # str(combined_res)
      
      detections <- combined_res$combined_segs
      # glimpse(detections)
      
      # If detections exist, iterate over these to get a clipping score and make a selection table
      if(nrow(detections) > 0 & !all(is.na(detections$onsets)) & !all(is.na(detections$offsets))){
        clipd <- sapply(1:nrow(detections), function(z){
          get_clip_score(dats[detections[z, "onsets"]:detections[z, "offsets"]])
        })
        
        # Get starts and ends
        starts <- tm[detections$onsets]
        ends <- tm[detections$offsets]
        
        # Make and return a selection table for warbleR analyses downstream
        detections <- data.frame(sound.files = rep(wavs[w], length(starts))) %>%
          dplyr::mutate(
            selec = seq(1, nrow(detections), 1),
            start = starts,
            end = ends,
            clipped = clipd,
            selection_length = ends - starts
          )
      } else {
        detections <- data.frame(sound.files = wavs[w]) %>%
          dplyr::mutate(
            selec = NA,
            start = NA,
            end = NA,
            clipped = NA,
            selection_length = NA
          )
      }
      
      # Return detections for the given sound file
      # Write a unique csv per sound file; remember to put all recordings in single directory on OSC
      write.csv(detections, file = file.path(outpath, paste("automatic_detections_", wavs[w], ".csv", sep = "")), row.names = FALSE)
    } else {
      if(!file.exists(WavError)) {
      cat(paste(wavs[w], "could not be read into R\n"), file = WavError, append = FALSE)
      } else if(file.exists(WavError)) {
        cat(paste(wavs[w], "could not be read into R\n"), file = WavError, append = TRUE)
      }
    }
   
    
  }))
  
}