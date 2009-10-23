-record(chunk, {
          id,            % Chunk ID
          ratio,         % Maximum Requested chunk ratio by fileservers
          storages = [], % List of storage URLs where chunk is stored
	  ref_cnt = 0    % Number of references registered to this chunk
        }).


-record(storage, {
          url,            % storage URL
          pid,            % Pid of storage process
          ref,            % Monitor Reference
          priority = 100, % Priority of storage
          read_speed,     % Last read speed (bytes/sec)
          write_speed     % Last write speed (bytes/sec)
         }).

