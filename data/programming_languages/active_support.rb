require 'active_support'
require 'active_support/time'
require 'active_support/core_ext'
module ActiveSupport
  # Wrapping an array in an +ArrayInquirer+ gives a friendlier way to check
  # its string-like contents:
  #
  #   variants = ActiveSupport::ArrayInquirer.new([:phone, :tablet])
  #
  #   variants.phone?    # => true
  #   variants.tablet?   # => true
  #   variants.desktop?  # => false
  #
  #   variants.any?(:phone, :tablet)   # => true
  #   variants.any?(:phone, :desktop)  # => true
  #   variants.any?(:desktop, :watch)  # => false
  class ArrayInquirer < Array
    def any?(*candidates, &block)
      if candidates.none?
        super
      else
        candidates.any? do |candidate|
          include?(candidate) || include?(candidate.to_sym)
        end
      end
    end

    private
      def respond_to_missing?(name, include_private = false)
        name[-1] == '?'
      end

      def method_missing(name, *args)
        if name[-1] == '?'
          any?(name[0..-2])
        else
          super
        end
      end
  end
end
module ActiveSupport
  # Backtraces often include many lines that are not relevant for the context
  # under review. This makes it hard to find the signal amongst the backtrace
  # noise, and adds debugging time. With a BacktraceCleaner, filters and
  # silencers are used to remove the noisy lines, so that only the most relevant
  # lines remain.
  #
  # Filters are used to modify lines of data, while silencers are used to remove
  # lines entirely. The typical filter use case is to remove lengthy path
  # information from the start of each line, and view file paths relevant to the
  # app directory instead of the file system root. The typical silencer use case
  # is to exclude the output of a noisy library from the backtrace, so that you
  # can focus on the rest.
  #
  #   bc = BacktraceCleaner.new
  #   bc.add_filter   { |line| line.gsub(Rails.root.to_s, '') } # strip the Rails.root prefix
  #   bc.add_silencer { |line| line =~ /mongrel|rubygems/ } # skip any lines from mongrel or rubygems
  #   bc.clean(exception.backtrace) # perform the cleanup
  #
  # To reconfigure an existing BacktraceCleaner (like the default one in Rails)
  # and show as much data as possible, you can always call
  # <tt>BacktraceCleaner#remove_silencers!</tt>, which will restore the
  # backtrace to a pristine state. If you need to reconfigure an existing
  # BacktraceCleaner so that it does not filter or modify the paths of any lines
  # of the backtrace, you can call <tt>BacktraceCleaner#remove_filters!</tt>
  # These two methods will give you a completely untouched backtrace.
  #
  # Inspired by the Quiet Backtrace gem by Thoughtbot.
  class BacktraceCleaner
    def initialize
      @filters, @silencers = [], []
    end

    # Returns the backtrace after all filters and silencers have been run
    # against it. Filters run first, then silencers.
    def clean(backtrace, kind = :silent)
      filtered = filter_backtrace(backtrace)

      case kind
      when :silent
        silence(filtered)
      when :noise
        noise(filtered)
      else
        filtered
      end
    end
    alias :filter :clean

    # Adds a filter from the block provided. Each line in the backtrace will be
    # mapped against this filter.
    #
    #   # Will turn "/my/rails/root/app/models/person.rb" into "/app/models/person.rb"
    #   backtrace_cleaner.add_filter { |line| line.gsub(Rails.root, '') }
    def add_filter(&block)
      @filters << block
    end

    # Adds a silencer from the block provided. If the silencer returns +true+
    # for a given line, it will be excluded from the clean backtrace.
    #
    #   # Will reject all lines that include the word "mongrel", like "/gems/mongrel/server.rb" or "/app/my_mongrel_server/rb"
    #   backtrace_cleaner.add_silencer { |line| line =~ /mongrel/ }
    def add_silencer(&block)
      @silencers << block
    end

    # Removes all silencers, but leaves in the filters. Useful if your
    # context of debugging suddenly expands as you suspect a bug in one of
    # the libraries you use.
    def remove_silencers!
      @silencers = []
    end

    # Removes all filters, but leaves in the silencers. Useful if you suddenly
    # need to see entire filepaths in the backtrace that you had already
    # filtered out.
    def remove_filters!
      @filters = []
    end

    private
      def filter_backtrace(backtrace)
        @filters.each do |f|
          backtrace = backtrace.map { |line| f.call(line) }
        end

        backtrace
      end

      def silence(backtrace)
        @silencers.each do |s|
          backtrace = backtrace.reject { |line| s.call(line) }
        end

        backtrace
      end

      def noise(backtrace)
        backtrace - silence(backtrace)
      end
  end
end
require 'active_support/core_ext/benchmark'
require 'active_support/core_ext/hash/keys'

module ActiveSupport
  module Benchmarkable
    # Allows you to measure the execution time of a block in a template and
    # records the result to the log. Wrap this block around expensive operations
    # or possible bottlenecks to get a time reading for the operation. For
    # example, let's say you thought your file processing method was taking too
    # long; you could wrap it in a benchmark block.
    #
    #  <% benchmark 'Process data files' do %>
    #    <%= expensive_files_operation %>
    #  <% end %>
    #
    # That would add something like "Process data files (345.2ms)" to the log,
    # which you can then use to compare timings when optimizing your code.
    #
    # You may give an optional logger level (<tt>:debug</tt>, <tt>:info</tt>,
    # <tt>:warn</tt>, <tt>:error</tt>) as the <tt>:level</tt> option. The
    # default logger level value is <tt>:info</tt>.
    #
    #  <% benchmark 'Low-level files', level: :debug do %>
    #    <%= lowlevel_files_operation %>
    #  <% end %>
    #
    # Finally, you can pass true as the third argument to silence all log
    # activity (other than the timing information) from inside the block. This
    # is great for boiling down a noisy block to just a single statement that
    # produces one log line:
    #
    #  <% benchmark 'Process data files', level: :info, silence: true do %>
    #    <%= expensive_and_chatty_files_operation %>
    #  <% end %>
    def benchmark(message = "Benchmarking", options = {})
      if logger
        options.assert_valid_keys(:level, :silence)
        options[:level] ||= :info

        result = nil
        ms = Benchmark.ms { result = options[:silence] ? silence { yield } : yield }
        logger.send(options[:level], '%s (%.1fms)' % [ message, ms ])
        result
      else
        yield
      end
    end
  end
end
begin
  require 'builder'
rescue LoadError => e
  $stderr.puts "You don't have builder installed in your application. Please add it to your Gemfile and run bundle install"
  raise e
end
require 'benchmark'
require 'zlib'
require 'active_support/core_ext/array/extract_options'
require 'active_support/core_ext/array/wrap'
require 'active_support/core_ext/benchmark'
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/core_ext/numeric/bytes'
require 'active_support/core_ext/numeric/time'
require 'active_support/core_ext/object/to_param'
require 'active_support/core_ext/string/inflections'

module ActiveSupport
  # See ActiveSupport::Cache::Store for documentation.
  module Cache
    autoload :FileStore,     'active_support/cache/file_store'
    autoload :MemoryStore,   'active_support/cache/memory_store'
    autoload :MemCacheStore, 'active_support/cache/mem_cache_store'
    autoload :NullStore,     'active_support/cache/null_store'

    # These options mean something to all cache implementations. Individual cache
    # implementations may support additional options.
    UNIVERSAL_OPTIONS = [:namespace, :compress, :compress_threshold, :expires_in, :race_condition_ttl]

    module Strategy
      autoload :LocalCache, 'active_support/cache/strategy/local_cache'
    end

    class << self
      # Creates a new CacheStore object according to the given options.
      #
      # If no arguments are passed to this method, then a new
      # ActiveSupport::Cache::MemoryStore object will be returned.
      #
      # If you pass a Symbol as the first argument, then a corresponding cache
      # store class under the ActiveSupport::Cache namespace will be created.
      # For example:
      #
      #   ActiveSupport::Cache.lookup_store(:memory_store)
      #   # => returns a new ActiveSupport::Cache::MemoryStore object
      #
      #   ActiveSupport::Cache.lookup_store(:mem_cache_store)
      #   # => returns a new ActiveSupport::Cache::MemCacheStore object
      #
      # Any additional arguments will be passed to the corresponding cache store
      # class's constructor:
      #
      #   ActiveSupport::Cache.lookup_store(:file_store, '/tmp/cache')
      #   # => same as: ActiveSupport::Cache::FileStore.new('/tmp/cache')
      #
      # If the first argument is not a Symbol, then it will simply be returned:
      #
      #   ActiveSupport::Cache.lookup_store(MyOwnCacheStore.new)
      #   # => returns MyOwnCacheStore.new
      def lookup_store(*store_option)
        store, *parameters = *Array.wrap(store_option).flatten

        case store
        when Symbol
          retrieve_store_class(store).new(*parameters)
        when nil
          ActiveSupport::Cache::MemoryStore.new
        else
          store
        end
      end

      # Expands out the +key+ argument into a key that can be used for the
      # cache store. Optionally accepts a namespace, and all keys will be
      # scoped within that namespace.
      #
      # If the +key+ argument provided is an array, or responds to +to_a+, then
      # each of elements in the array will be turned into parameters/keys and
      # concatenated into a single key. For example:
      #
      #   expand_cache_key([:foo, :bar])               # => "foo/bar"
      #   expand_cache_key([:foo, :bar], "namespace")  # => "namespace/foo/bar"
      #
      # The +key+ argument can also respond to +cache_key+ or +to_param+.
      def expand_cache_key(key, namespace = nil)
        expanded_cache_key = namespace ? "#{namespace}/" : ""

        if prefix = ENV["RAILS_CACHE_ID"] || ENV["RAILS_APP_VERSION"]
          expanded_cache_key << "#{prefix}/"
        end

        expanded_cache_key << retrieve_cache_key(key)
        expanded_cache_key
      end

      private
        def retrieve_cache_key(key)
          case
          when key.respond_to?(:cache_key) then key.cache_key
          when key.is_a?(Array)            then key.map { |element| retrieve_cache_key(element) }.to_param
          when key.respond_to?(:to_a)      then retrieve_cache_key(key.to_a)
          else                                  key.to_param
          end.to_s
        end

        # Obtains the specified cache store class, given the name of the +store+.
        # Raises an error when the store class cannot be found.
        def retrieve_store_class(store)
          require "active_support/cache/#{store}"
        rescue LoadError => e
          raise "Could not find cache store adapter for #{store} (#{e})"
        else
          ActiveSupport::Cache.const_get(store.to_s.camelize)
        end
    end

    # An abstract cache store class. There are multiple cache store
    # implementations, each having its own additional features. See the classes
    # under the ActiveSupport::Cache module, e.g.
    # ActiveSupport::Cache::MemCacheStore. MemCacheStore is currently the most
    # popular cache store for large production websites.
    #
    # Some implementations may not support all methods beyond the basic cache
    # methods of +fetch+, +write+, +read+, +exist?+, and +delete+.
    #
    # ActiveSupport::Cache::Store can store any serializable Ruby object.
    #
    #   cache = ActiveSupport::Cache::MemoryStore.new
    #
    #   cache.read('city')   # => nil
    #   cache.write('city', "Duckburgh")
    #   cache.read('city')   # => "Duckburgh"
    #
    # Keys are always translated into Strings and are case sensitive. When an
    # object is specified as a key and has a +cache_key+ method defined, this
    # method will be called to define the key.  Otherwise, the +to_param+
    # method will be called. Hashes and Arrays can also be used as keys. The
    # elements will be delimited by slashes, and the elements within a Hash
    # will be sorted by key so they are consistent.
    #
    #   cache.read('city') == cache.read(:city)   # => true
    #
    # Nil values can be cached.
    #
    # If your cache is on a shared infrastructure, you can define a namespace
    # for your cache entries. If a namespace is defined, it will be prefixed on
    # to every key. The namespace can be either a static value or a Proc. If it
    # is a Proc, it will be invoked when each key is evaluated so that you can
    # use application logic to invalidate keys.
    #
    #   cache.namespace = -> { @last_mod_time }  # Set the namespace to a variable
    #   @last_mod_time = Time.now  # Invalidate the entire cache by changing namespace
    #
    # Caches can also store values in a compressed format to save space and
    # reduce time spent sending data. Since there is overhead, values must be
    # large enough to warrant compression. To turn on compression either pass
    # <tt>compress: true</tt> in the initializer or as an option to +fetch+
    # or +write+. To specify the threshold at which to compress values, set the
    # <tt>:compress_threshold</tt> option. The default threshold is 16K.
    class Store
      cattr_accessor :logger, :instance_writer => true

      attr_reader :silence, :options
      alias :silence? :silence

      # Create a new cache. The options will be passed to any write method calls
      # except for <tt>:namespace</tt> which can be used to set the global
      # namespace for the cache.
      def initialize(options = nil)
        @options = options ? options.dup : {}
      end

      # Silence the logger.
      def silence!
        @silence = true
        self
      end

      # Silence the logger within a block.
      def mute
        previous_silence, @silence = defined?(@silence) && @silence, true
        yield
      ensure
        @silence = previous_silence
      end

      # Fetches data from the cache, using the given key. If there is data in
      # the cache with the given key, then that data is returned.
      #
      # If there is no such data in the cache (a cache miss), then +nil+ will be
      # returned. However, if a block has been passed, that block will be passed
      # the key and executed in the event of a cache miss. The return value of the
      # block will be written to the cache under the given cache key, and that
      # return value will be returned.
      #
      #   cache.write('today', 'Monday')
      #   cache.fetch('today')  # => "Monday"
      #
      #   cache.fetch('city')   # => nil
      #   cache.fetch('city') do
      #     'Duckburgh'
      #   end
      #   cache.fetch('city')   # => "Duckburgh"
      #
      # You may also specify additional options via the +options+ argument.
      # Setting <tt>force: true</tt> will force a cache miss:
      #
      #   cache.write('today', 'Monday')
      #   cache.fetch('today', force: true)  # => nil
      #
      # Setting <tt>:compress</tt> will store a large cache entry set by the call
      # in a compressed format.
      #
      # Setting <tt>:expires_in</tt> will set an expiration time on the cache.
      # All caches support auto-expiring content after a specified number of
      # seconds. This value can be specified as an option to the constructor
      # (in which case all entries will be affected), or it can be supplied to
      # the +fetch+ or +write+ method to effect just one entry.
      #
      #   cache = ActiveSupport::Cache::MemoryStore.new(expires_in: 5.minutes)
      #   cache.write(key, value, expires_in: 1.minute) # Set a lower value for one entry
      #
      # Setting <tt>:race_condition_ttl</tt> is very useful in situations where
      # a cache entry is used very frequently and is under heavy load. If a
      # cache expires and due to heavy load several different processes will try
      # to read data natively and then they all will try to write to cache. To
      # avoid that case the first process to find an expired cache entry will
      # bump the cache expiration time by the value set in <tt>:race_condition_ttl</tt>.
      # Yes, this process is extending the time for a stale value by another few
      # seconds. Because of extended life of the previous cache, other processes
      # will continue to use slightly stale data for a just a bit longer. In the
      # meantime that first process will go ahead and will write into cache the
      # new value. After that all the processes will start getting the new value.
      # The key is to keep <tt>:race_condition_ttl</tt> small.
      #
      # If the process regenerating the entry errors out, the entry will be
      # regenerated after the specified number of seconds. Also note that the
      # life of stale cache is extended only if it expired recently. Otherwise
      # a new value is generated and <tt>:race_condition_ttl</tt> does not play
      # any role.
      #
      #   # Set all values to expire after one minute.
      #   cache = ActiveSupport::Cache::MemoryStore.new(expires_in: 1.minute)
      #
      #   cache.write('foo', 'original value')
      #   val_1 = nil
      #   val_2 = nil
      #   sleep 60
      #
      #   Thread.new do
      #     val_1 = cache.fetch('foo', race_condition_ttl: 10) do
      #       sleep 1
      #       'new value 1'
      #     end
      #   end
      #
      #   Thread.new do
      #     val_2 = cache.fetch('foo', race_condition_ttl: 10) do
      #       'new value 2'
      #     end
      #   end
      #
      #   # val_1 => "new value 1"
      #   # val_2 => "original value"
      #   # sleep 10 # First thread extend the life of cache by another 10 seconds
      #   # cache.fetch('foo') => "new value 1"
      #
      # Other options will be handled by the specific cache store implementation.
      # Internally, #fetch calls #read_entry, and calls #write_entry on a cache
      # miss. +options+ will be passed to the #read and #write calls.
      #
      # For example, MemCacheStore's #write method supports the +:raw+
      # option, which tells the memcached server to store all values as strings.
      # We can use this option with #fetch too:
      #
      #   cache = ActiveSupport::Cache::MemCacheStore.new
      #   cache.fetch("foo", force: true, raw: true) do
      #     :bar
      #   end
      #   cache.fetch('foo') # => "bar"
      def fetch(name, options = nil)
        if block_given?
          options = merged_options(options)
          key = namespaced_key(name, options)

          cached_entry = find_cached_entry(key, name, options) unless options[:force]
          entry = handle_expired_entry(cached_entry, key, options)

          if entry
            get_entry_value(entry, name, options)
          else
            save_block_result_to_cache(name, options) { |_name| yield _name }
          end
        else
          read(name, options)
        end
      end

      # Fetches data from the cache, using the given key. If there is data in
      # the cache with the given key, then that data is returned. Otherwise,
      # +nil+ is returned.
      #
      # Options are passed to the underlying cache implementation.
      def read(name, options = nil)
        options = merged_options(options)
        key = namespaced_key(name, options)
        instrument(:read, name, options) do |payload|
          entry = read_entry(key, options)
          if entry
            if entry.expired?
              delete_entry(key, options)
              payload[:hit] = false if payload
              nil
            else
              payload[:hit] = true if payload
              entry.value
            end
          else
            payload[:hit] = false if payload
            nil
          end
        end
      end

      # Read multiple values at once from the cache. Options can be passed
      # in the last argument.
      #
      # Some cache implementation may optimize this method.
      #
      # Returns a hash mapping the names provided to the values found.
      def read_multi(*names)
        options = names.extract_options!
        options = merged_options(options)

        instrument_multi(:read, names, options) do |payload|
          results = {}
          names.each do |name|
            key = namespaced_key(name, options)
            entry = read_entry(key, options)
            if entry
              if entry.expired?
                delete_entry(key, options)
              else
                results[name] = entry.value
              end
            end
          end
          results
        end
      end

      # Fetches data from the cache, using the given keys. If there is data in
      # the cache with the given keys, then that data is returned. Otherwise,
      # the supplied block is called for each key for which there was no data,
      # and the result will be written to the cache and returned.
      #
      # Options are passed to the underlying cache implementation.
      #
      # Returns a hash with the data for each of the names. For example:
      #
      #   cache.write("bim", "bam")
      #   cache.fetch_multi("bim", "unknown_key") do |key|
      #     "Fallback value for key: #{key}"
      #   end
      #   # => { "bim" => "bam",
      #   #      "unknown_key" => "Fallback value for key: unknown_key" }
      #
      def fetch_multi(*names)
        options = names.extract_options!
        options = merged_options(options)
        results = read_multi(*names, options)

        names.each_with_object({}) do |name, memo|
          memo[name] = results.fetch(name) do
            value = yield name
            write(name, value, options)
            value
          end
        end
      end

      # Writes the value to the cache, with the key.
      #
      # Options are passed to the underlying cache implementation.
      def write(name, value, options = nil)
        options = merged_options(options)

        instrument(:write, name, options) do
          entry = Entry.new(value, options)
          write_entry(namespaced_key(name, options), entry, options)
        end
      end

      # Deletes an entry in the cache. Returns +true+ if an entry is deleted.
      #
      # Options are passed to the underlying cache implementation.
      def delete(name, options = nil)
        options = merged_options(options)

        instrument(:delete, name) do
          delete_entry(namespaced_key(name, options), options)
        end
      end

      # Returns +true+ if the cache contains an entry for the given key.
      #
      # Options are passed to the underlying cache implementation.
      def exist?(name, options = nil)
        options = merged_options(options)

        instrument(:exist?, name) do
          entry = read_entry(namespaced_key(name, options), options)
          (entry && !entry.expired?) || false
        end
      end

      # Delete all entries with keys matching the pattern.
      #
      # Options are passed to the underlying cache implementation.
      #
      # All implementations may not support this method.
      def delete_matched(matcher, options = nil)
        raise NotImplementedError.new("#{self.class.name} does not support delete_matched")
      end

      # Increment an integer value in the cache.
      #
      # Options are passed to the underlying cache implementation.
      #
      # All implementations may not support this method.
      def increment(name, amount = 1, options = nil)
        raise NotImplementedError.new("#{self.class.name} does not support increment")
      end

      # Decrement an integer value in the cache.
      #
      # Options are passed to the underlying cache implementation.
      #
      # All implementations may not support this method.
      def decrement(name, amount = 1, options = nil)
        raise NotImplementedError.new("#{self.class.name} does not support decrement")
      end

      # Cleanup the cache by removing expired entries.
      #
      # Options are passed to the underlying cache implementation.
      #
      # All implementations may not support this method.
      def cleanup(options = nil)
        raise NotImplementedError.new("#{self.class.name} does not support cleanup")
      end

      # Clear the entire cache. Be careful with this method since it could
      # affect other processes if shared cache is being used.
      #
      # The options hash is passed to the underlying cache implementation.
      #
      # All implementations may not support this method.
      def clear(options = nil)
        raise NotImplementedError.new("#{self.class.name} does not support clear")
      end

      protected
        # Add the namespace defined in the options to a pattern designed to
        # match keys. Implementations that support delete_matched should call
        # this method to translate a pattern that matches names into one that
        # matches namespaced keys.
        def key_matcher(pattern, options)
          prefix = options[:namespace].is_a?(Proc) ? options[:namespace].call : options[:namespace]
          if prefix
            source = pattern.source
            if source.start_with?('^')
              source = source[1, source.length]
            else
              source = ".*#{source[0, source.length]}"
            end
            Regexp.new("^#{Regexp.escape(prefix)}:#{source}", pattern.options)
          else
            pattern
          end
        end

        # Read an entry from the cache implementation. Subclasses must implement
        # this method.
        def read_entry(key, options) # :nodoc:
          raise NotImplementedError.new
        end

        # Write an entry to the cache implementation. Subclasses must implement
        # this method.
        def write_entry(key, entry, options) # :nodoc:
          raise NotImplementedError.new
        end

        # Delete an entry from the cache implementation. Subclasses must
        # implement this method.
        def delete_entry(key, options) # :nodoc:
          raise NotImplementedError.new
        end

      private
        # Merge the default options with ones specific to a method call.
        def merged_options(call_options) # :nodoc:
          if call_options
            options.merge(call_options)
          else
            options.dup
          end
        end

        # Expand key to be a consistent string value. Invoke +cache_key+ if
        # object responds to +cache_key+. Otherwise, +to_param+ method will be
        # called. If the key is a Hash, then keys will be sorted alphabetically.
        def expanded_key(key) # :nodoc:
          return key.cache_key.to_s if key.respond_to?(:cache_key)

          case key
          when Array
            if key.size > 1
              key = key.collect{|element| expanded_key(element)}
            else
              key = key.first
            end
          when Hash
            key = key.sort_by { |k,_| k.to_s }.collect{|k,v| "#{k}=#{v}"}
          end

          key.to_param
        end

        # Prefix a key with the namespace. Namespace and key will be delimited
        # with a colon.
        def namespaced_key(key, options)
          key = expanded_key(key)
          namespace = options[:namespace] if options
          prefix = namespace.is_a?(Proc) ? namespace.call : namespace
          key = "#{prefix}:#{key}" if prefix
          key
        end

        def instrument(operation, key, options = nil)
          log { "Cache #{operation}: #{key}#{options.blank? ? "" : " (#{options.inspect})"}" }

          payload = { :key => key }
          payload.merge!(options) if options.is_a?(Hash)
          ActiveSupport::Notifications.instrument("cache_#{operation}.active_support", payload){ yield(payload) }
        end

        def instrument_multi(operation, keys, options = nil)
          log do
            formatted_keys = keys.map { |k| "- #{k}" }.join("\n")
            "Caches multi #{operation}:\n#{formatted_keys}#{options.blank? ? "" : " (#{options.inspect})"}"
          end

          payload = { key: keys }
          payload.merge!(options) if options.is_a?(Hash)
          ActiveSupport::Notifications.instrument("cache_#{operation}_multi.active_support", payload) { yield(payload) }
        end

        def log
          return unless logger && logger.debug? && !silence?
          logger.debug(yield)
        end

        def find_cached_entry(key, name, options)
          instrument(:read, name, options) do |payload|
            payload[:super_operation] = :fetch if payload
            read_entry(key, options)
          end
        end

        def handle_expired_entry(entry, key, options)
          if entry && entry.expired?
            race_ttl = options[:race_condition_ttl].to_i
            if (race_ttl > 0) && (Time.now.to_f - entry.expires_at <= race_ttl)
              # When an entry has a positive :race_condition_ttl defined, put the stale entry back into the cache
              # for a brief period while the entry is being recalculated.
              entry.expires_at = Time.now + race_ttl
              write_entry(key, entry, :expires_in => race_ttl * 2)
            else
              delete_entry(key, options)
            end
            entry = nil
          end
          entry
        end

        def get_entry_value(entry, name, options)
          instrument(:fetch_hit, name, options) { |payload| }
          entry.value
        end

        def save_block_result_to_cache(name, options)
          result = instrument(:generate, name, options) do |payload|
            yield(name)
          end

          write(name, result, options)
          result
        end
    end

    # This class is used to represent cache entries. Cache entries have a value and an optional
    # expiration time. The expiration time is used to support the :race_condition_ttl option
    # on the cache.
    #
    # Since cache entries in most instances will be serialized, the internals of this class are highly optimized
    # using short instance variable names that are lazily defined.
    class Entry # :nodoc:
      DEFAULT_COMPRESS_LIMIT = 16.kilobytes

      # Create a new cache entry for the specified value. Options supported are
      # +:compress+, +:compress_threshold+, and +:expires_in+.
      def initialize(value, options = {})
        if should_compress?(value, options)
          @value = compress(value)
          @compressed = true
        else
          @value = value
        end

        @created_at = Time.now.to_f
        @expires_in = options[:expires_in]
        @expires_in = @expires_in.to_f if @expires_in
      end

      def value
        compressed? ? uncompress(@value) : @value
      end

      # Check if the entry is expired. The +expires_in+ parameter can override
      # the value set when the entry was created.
      def expired?
        @expires_in && @created_at + @expires_in <= Time.now.to_f
      end

      def expires_at
        @expires_in ? @created_at + @expires_in : nil
      end

      def expires_at=(value)
        if value
          @expires_in = value.to_f - @created_at
        else
          @expires_in = nil
        end
      end

      # Returns the size of the cached value. This could be less than
      # <tt>value.size</tt> if the data is compressed.
      def size
        if defined?(@s)
          @s
        else
          case value
          when NilClass
            0
          when String
            @value.bytesize
          else
            @s = Marshal.dump(@value).bytesize
          end
        end
      end

      # Duplicate the value in a class. This is used by cache implementations that don't natively
      # serialize entries to protect against accidental cache modifications.
      def dup_value!
        if @value && !compressed? && !(@value.is_a?(Numeric) || @value == true || @value == false)
          if @value.is_a?(String)
            @value = @value.dup
          else
            @value = Marshal.load(Marshal.dump(@value))
          end
        end
      end

      private
        def should_compress?(value, options)
          if value && options[:compress]
            compress_threshold = options[:compress_threshold] || DEFAULT_COMPRESS_LIMIT
            serialized_value_size = (value.is_a?(String) ? value : Marshal.dump(value)).bytesize

            return true if serialized_value_size >= compress_threshold
          end

          false
        end

        def compressed?
          defined?(@compressed) ? @compressed : false
        end

        def compress(value)
          Zlib::Deflate.deflate(Marshal.dump(value))
        end

        def uncompress(value)
          Marshal.load(Zlib::Inflate.inflate(value))
        end
    end
  end
end
require 'active_support/core_ext/marshal'
require 'active_support/core_ext/file/atomic'
require 'active_support/core_ext/string/conversions'
require 'uri/common'

module ActiveSupport
  module Cache
    # A cache store implementation which stores everything on the filesystem.
    #
    # FileStore implements the Strategy::LocalCache strategy which implements
    # an in-memory cache inside of a block.
    class FileStore < Store
      attr_reader :cache_path

      DIR_FORMATTER = "%03X"
      FILENAME_MAX_SIZE = 228 # max filename size on file system is 255, minus room for timestamp and random characters appended by Tempfile (used by atomic write)
      FILEPATH_MAX_SIZE = 900 # max is 1024, plus some room
      EXCLUDED_DIRS = ['.', '..'].freeze

      def initialize(cache_path, options = nil)
        super(options)
        @cache_path = cache_path.to_s
        extend Strategy::LocalCache
      end

      # Deletes all items from the cache. In this case it deletes all the entries in the specified
      # file store directory except for .gitkeep. Be careful which directory is specified in your
      # config file when using +FileStore+ because everything in that directory will be deleted.
      def clear(options = nil)
        root_dirs = Dir.entries(cache_path).reject {|f| (EXCLUDED_DIRS + [".gitkeep"]).include?(f)}
        FileUtils.rm_r(root_dirs.collect{|f| File.join(cache_path, f)})
      rescue Errno::ENOENT
      end

      # Preemptively iterates through all stored keys and removes the ones which have expired.
      def cleanup(options = nil)
        options = merged_options(options)
        search_dir(cache_path) do |fname|
          key = file_path_key(fname)
          entry = read_entry(key, options)
          delete_entry(key, options) if entry && entry.expired?
        end
      end

      # Increments an already existing integer value that is stored in the cache.
      # If the key is not found nothing is done.
      def increment(name, amount = 1, options = nil)
        modify_value(name, amount, options)
      end

      # Decrements an already existing integer value that is stored in the cache.
      # If the key is not found nothing is done.
      def decrement(name, amount = 1, options = nil)
        modify_value(name, -amount, options)
      end

      def delete_matched(matcher, options = nil)
        options = merged_options(options)
        instrument(:delete_matched, matcher.inspect) do
          matcher = key_matcher(matcher, options)
          search_dir(cache_path) do |path|
            key = file_path_key(path)
            delete_entry(key, options) if key.match(matcher)
          end
        end
      end

      protected

        def read_entry(key, options)
          file_name = key_file_path(key)
          if File.exist?(file_name)
            File.open(file_name) { |f| Marshal.load(f) }
          end
        rescue => e
          logger.error("FileStoreError (#{e}): #{e.message}") if logger
          nil
        end

        def write_entry(key, entry, options)
          file_name = key_file_path(key)
          return false if options[:unless_exist] && File.exist?(file_name)
          ensure_cache_path(File.dirname(file_name))
          File.atomic_write(file_name, cache_path) {|f| Marshal.dump(entry, f)}
          true
        end

        def delete_entry(key, options)
          file_name = key_file_path(key)
          if File.exist?(file_name)
            begin
              File.delete(file_name)
              delete_empty_directories(File.dirname(file_name))
              true
            rescue => e
              # Just in case the error was caused by another process deleting the file first.
              raise e if File.exist?(file_name)
              false
            end
          end
        end

      private
        # Lock a file for a block so only one process can modify it at a time.
        def lock_file(file_name, &block) # :nodoc:
          if File.exist?(file_name)
            File.open(file_name, 'r+') do |f|
              begin
                f.flock File::LOCK_EX
                yield
              ensure
                f.flock File::LOCK_UN
              end
            end
          else
            yield
          end
        end

        # Translate a key into a file path.
        def key_file_path(key)
          if key.size > FILEPATH_MAX_SIZE
            key = Digest::MD5.hexdigest(key)
          end

          fname = URI.encode_www_form_component(key)
          hash = Zlib.adler32(fname)
          hash, dir_1 = hash.divmod(0x1000)
          dir_2 = hash.modulo(0x1000)
          fname_paths = []

          # Make sure file name doesn't exceed file system limits.
          begin
            fname_paths << fname[0, FILENAME_MAX_SIZE]
            fname = fname[FILENAME_MAX_SIZE..-1]
          end until fname.blank?

          File.join(cache_path, DIR_FORMATTER % dir_1, DIR_FORMATTER % dir_2, *fname_paths)
        end

        # Translate a file path into a key.
        def file_path_key(path)
          fname = path[cache_path.to_s.size..-1].split(File::SEPARATOR, 4).last
          URI.decode_www_form_component(fname, Encoding::UTF_8)
        end

        # Delete empty directories in the cache.
        def delete_empty_directories(dir)
          return if File.realpath(dir) == File.realpath(cache_path)
          if Dir.entries(dir).reject {|f| EXCLUDED_DIRS.include?(f)}.empty?
            Dir.delete(dir) rescue nil
            delete_empty_directories(File.dirname(dir))
          end
        end

        # Make sure a file path's directories exist.
        def ensure_cache_path(path)
          FileUtils.makedirs(path) unless File.exist?(path)
        end

        def search_dir(dir, &callback)
          return if !File.exist?(dir)
          Dir.foreach(dir) do |d|
            next if EXCLUDED_DIRS.include?(d)
            name = File.join(dir, d)
            if File.directory?(name)
              search_dir(name, &callback)
            else
              callback.call name
            end
          end
        end

        # Modifies the amount of an already existing integer value that is stored in the cache.
        # If the key is not found nothing is done.
        def modify_value(name, amount, options)
          file_name = key_file_path(namespaced_key(name, options))

          lock_file(file_name) do
            options = merged_options(options)

            if num = read(name, options)
              num = num.to_i + amount
              write(name, num, options)
              num
            end
          end
        end
    end
  end
end
begin
  require 'dalli'
rescue LoadError => e
  $stderr.puts "You don't have dalli installed in your application. Please add it to your Gemfile and run bundle install"
  raise e
end

require 'digest/md5'
require 'active_support/core_ext/marshal'
require 'active_support/core_ext/array/extract_options'

module ActiveSupport
  module Cache
    # A cache store implementation which stores data in Memcached:
    # http://memcached.org/
    #
    # This is currently the most popular cache store for production websites.
    #
    # Special features:
    # - Clustering and load balancing. One can specify multiple memcached servers,
    #   and MemCacheStore will load balance between all available servers. If a
    #   server goes down, then MemCacheStore will ignore it until it comes back up.
    #
    # MemCacheStore implements the Strategy::LocalCache strategy which implements
    # an in-memory cache inside of a block.
    class MemCacheStore < Store
      ESCAPE_KEY_CHARS = /[\x00-\x20%\x7F-\xFF]/n

      def self.build_mem_cache(*addresses)
        addresses = addresses.flatten
        options = addresses.extract_options!
        addresses = ["localhost:11211"] if addresses.empty?
        Dalli::Client.new(addresses, options)
      end

      # Creates a new MemCacheStore object, with the given memcached server
      # addresses. Each address is either a host name, or a host-with-port string
      # in the form of "host_name:port". For example:
      #
      #   ActiveSupport::Cache::MemCacheStore.new("localhost", "server-downstairs.localnetwork:8229")
      #
      # If no addresses are specified, then MemCacheStore will connect to
      # localhost port 11211 (the default memcached port).
      def initialize(*addresses)
        addresses = addresses.flatten
        options = addresses.extract_options!
        super(options)

        unless [String, Dalli::Client, NilClass].include?(addresses.first.class)
          raise ArgumentError, "First argument must be an empty array, an array of hosts or a Dalli::Client instance."
        end
        if addresses.first.is_a?(Dalli::Client)
          @data = addresses.first
        else
          mem_cache_options = options.dup
          UNIVERSAL_OPTIONS.each{|name| mem_cache_options.delete(name)}
          @data = self.class.build_mem_cache(*(addresses + [mem_cache_options]))
        end

        extend Strategy::LocalCache
        extend LocalCacheWithRaw
      end

      # Reads multiple values from the cache using a single call to the
      # servers for all keys. Options can be passed in the last argument.
      def read_multi(*names)
        options = names.extract_options!
        options = merged_options(options)

        instrument_multi(:read, names, options) do
          keys_to_names = Hash[names.map{|name| [escape_key(namespaced_key(name, options)), name]}]
          raw_values = @data.get_multi(keys_to_names.keys, :raw => true)
          values = {}
          raw_values.each do |key, value|
            entry = deserialize_entry(value)
            values[keys_to_names[key]] = entry.value unless entry.expired?
          end
          values
        end
      end

      # Increment a cached value. This method uses the memcached incr atomic
      # operator and can only be used on values written with the :raw option.
      # Calling it on a value not stored with :raw will initialize that value
      # to zero.
      def increment(name, amount = 1, options = nil) # :nodoc:
        options = merged_options(options)
        instrument(:increment, name, :amount => amount) do
          @data.incr(escape_key(namespaced_key(name, options)), amount)
        end
      rescue Dalli::DalliError => e
        logger.error("DalliError (#{e}): #{e.message}") if logger
        nil
      end

      # Decrement a cached value. This method uses the memcached decr atomic
      # operator and can only be used on values written with the :raw option.
      # Calling it on a value not stored with :raw will initialize that value
      # to zero.
      def decrement(name, amount = 1, options = nil) # :nodoc:
        options = merged_options(options)
        instrument(:decrement, name, :amount => amount) do
          @data.decr(escape_key(namespaced_key(name, options)), amount)
        end
      rescue Dalli::DalliError => e
        logger.error("DalliError (#{e}): #{e.message}") if logger
        nil
      end

      # Clear the entire cache on all memcached servers. This method should
      # be used with care when shared cache is being used.
      def clear(options = nil)
        @data.flush_all
      rescue Dalli::DalliError => e
        logger.error("DalliError (#{e}): #{e.message}") if logger
        nil
      end

      # Get the statistics from the memcached servers.
      def stats
        @data.stats
      end

      protected
        # Read an entry from the cache.
        def read_entry(key, options) # :nodoc:
          deserialize_entry(@data.get(escape_key(key), options))
        rescue Dalli::DalliError => e
          logger.error("DalliError (#{e}): #{e.message}") if logger
          nil
        end

        # Write an entry to the cache.
        def write_entry(key, entry, options) # :nodoc:
          method = options && options[:unless_exist] ? :add : :set
          value = options[:raw] ? entry.value.to_s : entry
          expires_in = options[:expires_in].to_i
          if expires_in > 0 && !options[:raw]
            # Set the memcache expire a few minutes in the future to support race condition ttls on read
            expires_in += 5.minutes
          end
          @data.send(method, escape_key(key), value, expires_in, options)
        rescue Dalli::DalliError => e
          logger.error("DalliError (#{e}): #{e.message}") if logger
          false
        end

        # Delete an entry from the cache.
        def delete_entry(key, options) # :nodoc:
          @data.delete(escape_key(key))
        rescue Dalli::DalliError => e
          logger.error("DalliError (#{e}): #{e.message}") if logger
          false
        end

      private

        # Memcache keys are binaries. So we need to force their encoding to binary
        # before applying the regular expression to ensure we are escaping all
        # characters properly.
        def escape_key(key)
          key = key.to_s.dup
          key = key.force_encoding(Encoding::ASCII_8BIT)
          key = key.gsub(ESCAPE_KEY_CHARS){ |match| "%#{match.getbyte(0).to_s(16).upcase}" }
          key = "#{key[0, 213]}:md5:#{Digest::MD5.hexdigest(key)}" if key.size > 250
          key
        end

        def deserialize_entry(raw_value)
          if raw_value
            entry = Marshal.load(raw_value) rescue raw_value
            entry.is_a?(Entry) ? entry : Entry.new(entry)
          else
            nil
          end
        end

      # Provide support for raw values in the local cache strategy.
      module LocalCacheWithRaw # :nodoc:
        protected
          def read_entry(key, options)
            entry = super
            if options[:raw] && local_cache && entry
               entry = deserialize_entry(entry.value)
            end
            entry
          end

          def write_entry(key, entry, options) # :nodoc:
            retval = super
            if options[:raw] && local_cache && retval
              raw_entry = Entry.new(entry.value.to_s)
              raw_entry.expires_at = entry.expires_at
              local_cache.write_entry(key, raw_entry, options)
            end
            retval
          end
      end
    end
  end
end
require 'monitor'

module ActiveSupport
  module Cache
    # A cache store implementation which stores everything into memory in the
    # same process. If you're running multiple Ruby on Rails server processes
    # (which is the case if you're using mongrel_cluster or Phusion Passenger),
    # then this means that Rails server process instances won't be able
    # to share cache data with each other and this may not be the most
    # appropriate cache in that scenario.
    #
    # This cache has a bounded size specified by the :size options to the
    # initializer (default is 32Mb). When the cache exceeds the allotted size,
    # a cleanup will occur which tries to prune the cache down to three quarters
    # of the maximum size by removing the least recently used entries.
    #
    # MemoryStore is thread-safe.
    class MemoryStore < Store
      def initialize(options = nil)
        options ||= {}
        super(options)
        @data = {}
        @key_access = {}
        @max_size = options[:size] || 32.megabytes
        @max_prune_time = options[:max_prune_time] || 2
        @cache_size = 0
        @monitor = Monitor.new
        @pruning = false
      end

      def clear(options = nil)
        synchronize do
          @data.clear
          @key_access.clear
          @cache_size = 0
        end
      end

      # Preemptively iterates through all stored keys and removes the ones which have expired.
      def cleanup(options = nil)
        options = merged_options(options)
        instrument(:cleanup, :size => @data.size) do
          keys = synchronize{ @data.keys }
          keys.each do |key|
            entry = @data[key]
            delete_entry(key, options) if entry && entry.expired?
          end
        end
      end

      # To ensure entries fit within the specified memory prune the cache by removing the least
      # recently accessed entries.
      def prune(target_size, max_time = nil)
        return if pruning?
        @pruning = true
        begin
          start_time = Time.now
          cleanup
          instrument(:prune, target_size, :from => @cache_size) do
            keys = synchronize{ @key_access.keys.sort{|a,b| @key_access[a].to_f <=> @key_access[b].to_f} }
            keys.each do |key|
              delete_entry(key, options)
              return if @cache_size <= target_size || (max_time && Time.now - start_time > max_time)
            end
          end
        ensure
          @pruning = false
        end
      end

      # Returns true if the cache is currently being pruned.
      def pruning?
        @pruning
      end

      # Increment an integer value in the cache.
      def increment(name, amount = 1, options = nil)
        synchronize do
          options = merged_options(options)
          if num = read(name, options)
            num = num.to_i + amount
            write(name, num, options)
            num
          else
            nil
          end
        end
      end

      # Decrement an integer value in the cache.
      def decrement(name, amount = 1, options = nil)
        synchronize do
          options = merged_options(options)
          if num = read(name, options)
            num = num.to_i - amount
            write(name, num, options)
            num
          else
            nil
          end
        end
      end

      def delete_matched(matcher, options = nil)
        options = merged_options(options)
        instrument(:delete_matched, matcher.inspect) do
          matcher = key_matcher(matcher, options)
          keys = synchronize { @data.keys }
          keys.each do |key|
            delete_entry(key, options) if key.match(matcher)
          end
        end
      end

      def inspect # :nodoc:
        "<##{self.class.name} entries=#{@data.size}, size=#{@cache_size}, options=#{@options.inspect}>"
      end

      # Synchronize calls to the cache. This should be called wherever the underlying cache implementation
      # is not thread safe.
      def synchronize(&block) # :nodoc:
        @monitor.synchronize(&block)
      end

      protected

        PER_ENTRY_OVERHEAD = 240

        def cached_size(key, entry)
          key.to_s.bytesize + entry.size + PER_ENTRY_OVERHEAD
        end

        def read_entry(key, options) # :nodoc:
          entry = @data[key]
          synchronize do
            if entry
              @key_access[key] = Time.now.to_f
            else
              @key_access.delete(key)
            end
          end
          entry
        end

        def write_entry(key, entry, options) # :nodoc:
          entry.dup_value!
          synchronize do
            old_entry = @data[key]
            return false if @data.key?(key) && options[:unless_exist]
            if old_entry
              @cache_size -= (old_entry.size - entry.size)
            else
              @cache_size += cached_size(key, entry)
            end
            @key_access[key] = Time.now.to_f
            @data[key] = entry
            prune(@max_size * 0.75, @max_prune_time) if @cache_size > @max_size
            true
          end
        end

        def delete_entry(key, options) # :nodoc:
          synchronize do
            @key_access.delete(key)
            entry = @data.delete(key)
            @cache_size -= cached_size(key, entry) if entry
            !!entry
          end
        end
    end
  end
end
module ActiveSupport
  module Cache
    # A cache store implementation which doesn't actually store anything. Useful in
    # development and test environments where you don't want caching turned on but
    # need to go through the caching interface.
    #
    # This cache does implement the local cache strategy, so values will actually
    # be cached inside blocks that utilize this strategy. See
    # ActiveSupport::Cache::Strategy::LocalCache for more details.
    class NullStore < Store
      def initialize(options = nil)
        super(options)
        extend Strategy::LocalCache
      end

      def clear(options = nil)
      end

      def cleanup(options = nil)
      end

      def increment(name, amount = 1, options = nil)
      end

      def decrement(name, amount = 1, options = nil)
      end

      def delete_matched(matcher, options = nil)
      end

      protected
        def read_entry(key, options) # :nodoc:
        end

        def write_entry(key, entry, options) # :nodoc:
          true
        end

        def delete_entry(key, options) # :nodoc:
          false
        end
    end
  end
end
require 'active_support/core_ext/object/duplicable'
require 'active_support/core_ext/string/inflections'
require 'active_support/per_thread_registry'

module ActiveSupport
  module Cache
    module Strategy
      # Caches that implement LocalCache will be backed by an in-memory cache for the
      # duration of a block. Repeated calls to the cache for the same key will hit the
      # in-memory cache for faster access.
      module LocalCache
        autoload :Middleware, 'active_support/cache/strategy/local_cache_middleware'

        # Class for storing and registering the local caches.
        class LocalCacheRegistry # :nodoc:
          extend ActiveSupport::PerThreadRegistry

          def initialize
            @registry = {}
          end

          def cache_for(local_cache_key)
            @registry[local_cache_key]
          end

          def set_cache_for(local_cache_key, value)
            @registry[local_cache_key] = value
          end

          def self.set_cache_for(l, v); instance.set_cache_for l, v; end
          def self.cache_for(l); instance.cache_for l; end
        end

        # Simple memory backed cache. This cache is not thread safe and is intended only
        # for serving as a temporary memory cache for a single thread.
        class LocalStore < Store
          def initialize
            super
            @data = {}
          end

          # Don't allow synchronizing since it isn't thread safe.
          def synchronize # :nodoc:
            yield
          end

          def clear(options = nil)
            @data.clear
          end

          def read_entry(key, options)
            @data[key]
          end

          def write_entry(key, value, options)
            @data[key] = value
            true
          end

          def delete_entry(key, options)
            !!@data.delete(key)
          end
        end

        # Use a local cache for the duration of block.
        def with_local_cache
          use_temporary_local_cache(LocalStore.new) { yield }
        end
        # Middleware class can be inserted as a Rack handler to be local cache for the
        # duration of request.
        def middleware
          @middleware ||= Middleware.new(
            "ActiveSupport::Cache::Strategy::LocalCache",
            local_cache_key)
        end

        def clear(options = nil) # :nodoc:
          local_cache.clear(options) if local_cache
          super
        end

        def cleanup(options = nil) # :nodoc:
          local_cache.clear(options) if local_cache
          super
        end

        def increment(name, amount = 1, options = nil) # :nodoc:
          value = bypass_local_cache{super}
          set_cache_value(value, name, amount, options)
          value
        end

        def decrement(name, amount = 1, options = nil) # :nodoc:
          value = bypass_local_cache{super}
          set_cache_value(value, name, amount, options)
          value
        end

        protected
          def read_entry(key, options) # :nodoc:
            if local_cache
              entry = local_cache.read_entry(key, options)
              unless entry
                entry = super
                local_cache.write_entry(key, entry, options)
              end
              entry
            else
              super
            end
          end

          def write_entry(key, entry, options) # :nodoc:
            local_cache.write_entry(key, entry, options) if local_cache
            super
          end

          def delete_entry(key, options) # :nodoc:
            local_cache.delete_entry(key, options) if local_cache
            super
          end

          def set_cache_value(value, name, amount, options)
            if local_cache
              local_cache.mute do
                if value
                  local_cache.write(name, value, options)
                else
                  local_cache.delete(name, options)
                end
              end
            end
          end

        private

          def local_cache_key
            @local_cache_key ||= "#{self.class.name.underscore}_local_cache_#{object_id}".gsub(/[\/-]/, '_').to_sym
          end

          def local_cache
            LocalCacheRegistry.cache_for(local_cache_key)
          end

          def bypass_local_cache
            use_temporary_local_cache(nil) { yield }
          end

          def use_temporary_local_cache(temporary_cache)
            save_cache = LocalCacheRegistry.cache_for(local_cache_key)
            begin
              LocalCacheRegistry.set_cache_for(local_cache_key, temporary_cache)
              yield
            ensure
              LocalCacheRegistry.set_cache_for(local_cache_key, save_cache)
            end
          end
      end
    end
  end
end
require 'rack/body_proxy'
require 'rack/utils'

module ActiveSupport
  module Cache
    module Strategy
      module LocalCache

        #--
        # This class wraps up local storage for middlewares. Only the middleware method should
        # construct them.
        class Middleware # :nodoc:
          attr_reader :name, :local_cache_key

          def initialize(name, local_cache_key)
            @name             = name
            @local_cache_key = local_cache_key
            @app              = nil
          end

          def new(app)
            @app = app
            self
          end

          def call(env)
            LocalCacheRegistry.set_cache_for(local_cache_key, LocalStore.new)
            response = @app.call(env)
            response[2] = ::Rack::BodyProxy.new(response[2]) do
              LocalCacheRegistry.set_cache_for(local_cache_key, nil)
            end
            response
          rescue Rack::Utils::InvalidParameterError
            LocalCacheRegistry.set_cache_for(local_cache_key, nil)
            [400, {}, []]
          rescue Exception
            LocalCacheRegistry.set_cache_for(local_cache_key, nil)
            raise
          end
        end
      end
    end
  end
end
require 'active_support/concern'
require 'active_support/descendants_tracker'
require 'active_support/core_ext/array/extract_options'
require 'active_support/core_ext/class/attribute'
require 'active_support/core_ext/kernel/reporting'
require 'active_support/core_ext/kernel/singleton_class'
require 'active_support/core_ext/string/filters'
require 'active_support/deprecation'
require 'thread'

module ActiveSupport
  # Callbacks are code hooks that are run at key points in an object's life cycle.
  # The typical use case is to have a base class define a set of callbacks
  # relevant to the other functionality it supplies, so that subclasses can
  # install callbacks that enhance or modify the base functionality without
  # needing to override or redefine methods of the base class.
  #
  # Mixing in this module allows you to define the events in the object's
  # life cycle that will support callbacks (via +ClassMethods.define_callbacks+),
  # set the instance methods, procs, or callback objects to be called (via
  # +ClassMethods.set_callback+), and run the installed callbacks at the
  # appropriate times (via +run_callbacks+).
  #
  # Three kinds of callbacks are supported: before callbacks, run before a
  # certain event; after callbacks, run after the event; and around callbacks,
  # blocks that surround the event, triggering it when they yield. Callback code
  # can be contained in instance methods, procs or lambdas, or callback objects
  # that respond to certain predetermined methods. See +ClassMethods.set_callback+
  # for details.
  #
  #   class Record
  #     include ActiveSupport::Callbacks
  #     define_callbacks :save
  #
  #     def save
  #       run_callbacks :save do
  #         puts "- save"
  #       end
  #     end
  #   end
  #
  #   class PersonRecord < Record
  #     set_callback :save, :before, :saving_message
  #     def saving_message
  #       puts "saving..."
  #     end
  #
  #     set_callback :save, :after do |object|
  #       puts "saved"
  #     end
  #   end
  #
  #   person = PersonRecord.new
  #   person.save
  #
  # Output:
  #   saving...
  #   - save
  #   saved
  module Callbacks
    extend Concern

    included do
      extend ActiveSupport::DescendantsTracker
    end

    CALLBACK_FILTER_TYPES = [:before, :after, :around]

    # Runs the callbacks for the given event.
    #
    # Calls the before and around callbacks in the order they were set, yields
    # the block (if given one), and then runs the after callbacks in reverse
    # order.
    #
    # If the callback chain was halted, returns +false+. Otherwise returns the
    # result of the block, +nil+ if no callbacks have been set, or +true+
    # if callbacks have been set but no block is given.
    #
    #   run_callbacks :save do
    #     save
    #   end
    def run_callbacks(kind, &block)
      callbacks = send("_#{kind}_callbacks")

      if callbacks.empty?
        yield if block_given?
      else
        runner = callbacks.compile
        e = Filters::Environment.new(self, false, nil, block)
        runner.call(e).value
      end
    end

    private

    # A hook invoked every time a before callback is halted.
    # This can be overridden in AS::Callback implementors in order
    # to provide better debugging/logging.
    def halted_callback_hook(filter)
    end

    module Conditionals # :nodoc:
      class Value
        def initialize(&block)
          @block = block
        end
        def call(target, value); @block.call(value); end
      end
    end

    module Filters
      Environment = Struct.new(:target, :halted, :value, :run_block)

      class End
        def call(env)
          block = env.run_block
          env.value = !env.halted && (!block || block.call)
          env
        end
      end
      ENDING = End.new

      class Before
        def self.build(callback_sequence, user_callback, user_conditions, chain_config, filter)
          halted_lambda = chain_config[:terminator]

          if chain_config.key?(:terminator) && user_conditions.any?
            halting_and_conditional(callback_sequence, user_callback, user_conditions, halted_lambda, filter)
          elsif chain_config.key? :terminator
            halting(callback_sequence, user_callback, halted_lambda, filter)
          elsif user_conditions.any?
            conditional(callback_sequence, user_callback, user_conditions)
          else
            simple callback_sequence, user_callback
          end
        end

        def self.halting_and_conditional(callback_sequence, user_callback, user_conditions, halted_lambda, filter)
          callback_sequence.before do |env|
            target = env.target
            value  = env.value
            halted = env.halted

            if !halted && user_conditions.all? { |c| c.call(target, value) }
              result_lambda = -> { user_callback.call target, value }
              env.halted = halted_lambda.call(target, result_lambda)
              if env.halted
                target.send :halted_callback_hook, filter
              end
            end

            env
          end
        end
        private_class_method :halting_and_conditional

        def self.halting(callback_sequence, user_callback, halted_lambda, filter)
          callback_sequence.before do |env|
            target = env.target
            value  = env.value
            halted = env.halted

            unless halted
              result_lambda = -> { user_callback.call target, value }
              env.halted = halted_lambda.call(target, result_lambda)

              if env.halted
                target.send :halted_callback_hook, filter
              end
            end

            env
          end
        end
        private_class_method :halting

        def self.conditional(callback_sequence, user_callback, user_conditions)
          callback_sequence.before do |env|
            target = env.target
            value  = env.value

            if user_conditions.all? { |c| c.call(target, value) }
              user_callback.call target, value
            end

            env
          end
        end
        private_class_method :conditional

        def self.simple(callback_sequence, user_callback)
          callback_sequence.before do |env|
            user_callback.call env.target, env.value

            env
          end
        end
        private_class_method :simple
      end

      class After
        def self.build(callback_sequence, user_callback, user_conditions, chain_config)
          if chain_config[:skip_after_callbacks_if_terminated]
            if chain_config.key?(:terminator) && user_conditions.any?
              halting_and_conditional(callback_sequence, user_callback, user_conditions)
            elsif chain_config.key?(:terminator)
              halting(callback_sequence, user_callback)
            elsif user_conditions.any?
              conditional callback_sequence, user_callback, user_conditions
            else
              simple callback_sequence, user_callback
            end
          else
            if user_conditions.any?
              conditional callback_sequence, user_callback, user_conditions
            else
              simple callback_sequence, user_callback
            end
          end
        end

        def self.halting_and_conditional(callback_sequence, user_callback, user_conditions)
          callback_sequence.after do |env|
            target = env.target
            value  = env.value
            halted = env.halted

            if !halted && user_conditions.all? { |c| c.call(target, value) }
              user_callback.call target, value
            end

            env
          end
        end
        private_class_method :halting_and_conditional

        def self.halting(callback_sequence, user_callback)
          callback_sequence.after do |env|
            unless env.halted
              user_callback.call env.target, env.value
            end

            env
          end
        end
        private_class_method :halting

        def self.conditional(callback_sequence, user_callback, user_conditions)
          callback_sequence.after do |env|
            target = env.target
            value  = env.value

            if user_conditions.all? { |c| c.call(target, value) }
              user_callback.call target, value
            end

            env
          end
        end
        private_class_method :conditional

        def self.simple(callback_sequence, user_callback)
          callback_sequence.after do |env|
            user_callback.call env.target, env.value

            env
          end
        end
        private_class_method :simple
      end

      class Around
        def self.build(callback_sequence, user_callback, user_conditions, chain_config)
          if chain_config.key?(:terminator) && user_conditions.any?
            halting_and_conditional(callback_sequence, user_callback, user_conditions)
          elsif chain_config.key? :terminator
            halting(callback_sequence, user_callback)
          elsif user_conditions.any?
            conditional(callback_sequence, user_callback, user_conditions)
          else
            simple(callback_sequence, user_callback)
          end
        end

        def self.halting_and_conditional(callback_sequence, user_callback, user_conditions)
          callback_sequence.around do |env, &run|
            target = env.target
            value  = env.value
            halted = env.halted

            if !halted && user_conditions.all? { |c| c.call(target, value) }
              user_callback.call(target, value) {
                run.call.value
              }
              env
            else
              run.call
            end
          end
        end
        private_class_method :halting_and_conditional

        def self.halting(callback_sequence, user_callback)
          callback_sequence.around do |env, &run|
            target = env.target
            value  = env.value

            if env.halted
              run.call
            else
              user_callback.call(target, value) {
                run.call.value
              }
              env
            end
          end
        end
        private_class_method :halting

        def self.conditional(callback_sequence, user_callback, user_conditions)
          callback_sequence.around do |env, &run|
            target = env.target
            value  = env.value

            if user_conditions.all? { |c| c.call(target, value) }
              user_callback.call(target, value) {
                run.call.value
              }
              env
            else
              run.call
            end
          end
        end
        private_class_method :conditional

        def self.simple(callback_sequence, user_callback)
          callback_sequence.around do |env, &run|
            user_callback.call(env.target, env.value) {
              run.call.value
            }
            env
          end
        end
        private_class_method :simple
      end
    end

    class Callback #:nodoc:#
      def self.build(chain, filter, kind, options)
        new chain.name, filter, kind, options, chain.config
      end

      attr_accessor :kind, :name
      attr_reader :chain_config

      def initialize(name, filter, kind, options, chain_config)
        @chain_config  = chain_config
        @name    = name
        @kind    = kind
        @filter  = filter
        @key     = compute_identifier filter
        @if      = Array(options[:if])
        @unless  = Array(options[:unless])
      end

      def filter; @key; end
      def raw_filter; @filter; end

      def merge_conditional_options(chain, if_option:, unless_option:)
        options = {
          :if     => @if.dup,
          :unless => @unless.dup
        }

        options[:if].concat     Array(unless_option)
        options[:unless].concat Array(if_option)

        self.class.build chain, @filter, @kind, options
      end

      def matches?(_kind, _filter)
        @kind == _kind && filter == _filter
      end

      def duplicates?(other)
        case @filter
        when Symbol, String
          matches?(other.kind, other.filter)
        else
          false
        end
      end

      # Wraps code with filter
      def apply(callback_sequence)
        user_conditions = conditions_lambdas
        user_callback = make_lambda @filter

        case kind
        when :before
          Filters::Before.build(callback_sequence, user_callback, user_conditions, chain_config, @filter)
        when :after
          Filters::After.build(callback_sequence, user_callback, user_conditions, chain_config)
        when :around
          Filters::Around.build(callback_sequence, user_callback, user_conditions, chain_config)
        end
      end

      private

      def invert_lambda(l)
        lambda { |*args, &blk| !l.call(*args, &blk) }
      end

      # Filters support:
      #
      #   Symbols:: A method to call.
      #   Strings:: Some content to evaluate.
      #   Procs::   A proc to call with the object.
      #   Objects:: An object with a <tt>before_foo</tt> method on it to call.
      #
      # All of these objects are converted into a lambda and handled
      # the same after this point.
      def make_lambda(filter)
        case filter
        when Symbol
          lambda { |target, _, &blk| target.send filter, &blk }
        when String
          l = eval "lambda { |value| #{filter} }"
          lambda { |target, value| target.instance_exec(value, &l) }
        when Conditionals::Value then filter
        when ::Proc
          if filter.arity > 1
            return lambda { |target, _, &block|
              raise ArgumentError unless block
              target.instance_exec(target, block, &filter)
            }
          end

          if filter.arity <= 0
            lambda { |target, _| target.instance_exec(&filter) }
          else
            lambda { |target, _| target.instance_exec(target, &filter) }
          end
        else
          scopes = Array(chain_config[:scope])
          method_to_call = scopes.map{ |s| public_send(s) }.join("_")

          lambda { |target, _, &blk|
            filter.public_send method_to_call, target, &blk
          }
        end
      end

      def compute_identifier(filter)
        case filter
        when String, ::Proc
          filter.object_id
        else
          filter
        end
      end

      def conditions_lambdas
        @if.map { |c| make_lambda c } +
          @unless.map { |c| invert_lambda make_lambda c }
      end
    end

    # Execute before and after filters in a sequence instead of
    # chaining them with nested lambda calls, see:
    # https://github.com/rails/rails/issues/18011
    class CallbackSequence
      def initialize(&call)
        @call = call
        @before = []
        @after = []
      end

      def before(&before)
        @before.unshift(before)
        self
      end

      def after(&after)
        @after.push(after)
        self
      end

      def around(&around)
        CallbackSequence.new do |arg|
          around.call(arg) {
            self.call(arg)
          }
        end
      end

      def call(arg)
        @before.each { |b| b.call(arg) }
        value = @call.call(arg)
        @after.each { |a| a.call(arg) }
        value
      end
    end

    # An Array with a compile method.
    class CallbackChain #:nodoc:#
      include Enumerable

      attr_reader :name, :config

      # If true, any callback returning +false+ will halt the entire callback
      # chain and display a deprecation message. If false, callback chains will
      # only be halted by calling +throw :abort+. Defaults to +true+.
      class_attribute :halt_and_display_warning_on_return_false
      self.halt_and_display_warning_on_return_false = true

      def initialize(name, config)
        @name = name
        @config = {
          scope: [:kind],
          terminator: default_terminator
        }.merge!(config)
        @chain = []
        @callbacks = nil
        @mutex = Mutex.new
      end

      def each(&block); @chain.each(&block); end
      def index(o);     @chain.index(o); end
      def empty?;       @chain.empty?; end

      def insert(index, o)
        @callbacks = nil
        @chain.insert(index, o)
      end

      def delete(o)
        @callbacks = nil
        @chain.delete(o)
      end

      def clear
        @callbacks = nil
        @chain.clear
        self
      end

      def initialize_copy(other)
        @callbacks = nil
        @chain     = other.chain.dup
        @mutex     = Mutex.new
      end

      def compile
        @callbacks || @mutex.synchronize do
          final_sequence = CallbackSequence.new { |env| Filters::ENDING.call(env) }
          @callbacks ||= @chain.reverse.inject(final_sequence) do |callback_sequence, callback|
            callback.apply callback_sequence
          end
        end
      end

      def append(*callbacks)
        callbacks.each { |c| append_one(c) }
      end

      def prepend(*callbacks)
        callbacks.each { |c| prepend_one(c) }
      end

      protected
      def chain; @chain; end

      private

      def append_one(callback)
        @callbacks = nil
        remove_duplicates(callback)
        @chain.push(callback)
      end

      def prepend_one(callback)
        @callbacks = nil
        remove_duplicates(callback)
        @chain.unshift(callback)
      end

      def remove_duplicates(callback)
        @callbacks = nil
        @chain.delete_if { |c| callback.duplicates?(c) }
      end

      def default_terminator
        Proc.new do |target, result_lambda|
          terminate = true
          catch(:abort) do
            result = result_lambda.call if result_lambda.is_a?(Proc)
            if halt_and_display_warning_on_return_false && result == false
              display_deprecation_warning_for_false_terminator
            else
              terminate = false
            end
          end
          terminate
        end
      end

      def display_deprecation_warning_for_false_terminator
        ActiveSupport::Deprecation.warn(<<-MSG.squish)
          Returning `false` in a callback will not implicitly halt a callback chain in the next release of Rails.
          To explicitly halt a callback chain, please use `throw :abort` instead.
        MSG
      end
    end

    module ClassMethods
      def normalize_callback_params(filters, block) # :nodoc:
        type = CALLBACK_FILTER_TYPES.include?(filters.first) ? filters.shift : :before
        options = filters.extract_options!
        filters.unshift(block) if block
        [type, filters, options.dup]
      end

      # This is used internally to append, prepend and skip callbacks to the
      # CallbackChain.
      def __update_callbacks(name) #:nodoc:
        ([self] + ActiveSupport::DescendantsTracker.descendants(self)).reverse_each do |target|
          chain = target.get_callbacks name
          yield target, chain.dup
        end
      end

      # Install a callback for the given event.
      #
      #   set_callback :save, :before, :before_meth
      #   set_callback :save, :after,  :after_meth, if: :condition
      #   set_callback :save, :around, ->(r, block) { stuff; result = block.call; stuff }
      #
      # The second argument indicates whether the callback is to be run +:before+,
      # +:after+, or +:around+ the event. If omitted, +:before+ is assumed. This
      # means the first example above can also be written as:
      #
      #   set_callback :save, :before_meth
      #
      # The callback can be specified as a symbol naming an instance method; as a
      # proc, lambda, or block; as a string to be instance evaluated; or as an
      # object that responds to a certain method determined by the <tt>:scope</tt>
      # argument to +define_callbacks+.
      #
      # If a proc, lambda, or block is given, its body is evaluated in the context
      # of the current object. It can also optionally accept the current object as
      # an argument.
      #
      # Before and around callbacks are called in the order that they are set;
      # after callbacks are called in the reverse order.
      #
      # Around callbacks can access the return value from the event, if it
      # wasn't halted, from the +yield+ call.
      #
      # ===== Options
      #
      # * <tt>:if</tt> - A symbol, a string or an array of symbols and strings,
      #   each naming an instance method or a proc; the callback will be called
      #   only when they all return a true value.
      # * <tt>:unless</tt> - A symbol, a string or an array of symbols and
      #   strings, each naming an instance method or a proc; the callback will
      #   be called only when they all return a false value.
      # * <tt>:prepend</tt> - If +true+, the callback will be prepended to the
      #   existing chain rather than appended.
      def set_callback(name, *filter_list, &block)
        type, filters, options = normalize_callback_params(filter_list, block)
        self_chain = get_callbacks name
        mapped = filters.map do |filter|
          Callback.build(self_chain, filter, type, options)
        end

        __update_callbacks(name) do |target, chain|
          options[:prepend] ? chain.prepend(*mapped) : chain.append(*mapped)
          target.set_callbacks name, chain
        end
      end

      # Skip a previously set callback. Like +set_callback+, <tt>:if</tt> or
      # <tt>:unless</tt> options may be passed in order to control when the
      # callback is skipped.
      #
      #   class Writer < Person
      #      skip_callback :validate, :before, :check_membership, if: -> { self.age > 18 }
      #   end
      #
      # An <tt>ArgumentError</tt> will be raised if the callback has not
      # already been set (unless the <tt>:raise</tt> option is set to <tt>false</tt>).
      def skip_callback(name, *filter_list, &block)
        type, filters, options = normalize_callback_params(filter_list, block)
        options[:raise] = true unless options.key?(:raise)

        __update_callbacks(name) do |target, chain|
          filters.each do |filter|
            callback = chain.find {|c| c.matches?(type, filter) }

            if !callback && options[:raise]
              raise ArgumentError, "#{type.to_s.capitalize} #{name} callback #{filter.inspect} has not been defined"
            end

            if callback && (options.key?(:if) || options.key?(:unless))
              new_callback = callback.merge_conditional_options(chain, if_option: options[:if], unless_option: options[:unless])
              chain.insert(chain.index(callback), new_callback)
            end

            chain.delete(callback)
          end
          target.set_callbacks name, chain
        end
      end

      # Remove all set callbacks for the given event.
      def reset_callbacks(name)
        callbacks = get_callbacks name

        ActiveSupport::DescendantsTracker.descendants(self).each do |target|
          chain = target.get_callbacks(name).dup
          callbacks.each { |c| chain.delete(c) }
          target.set_callbacks name, chain
        end

        self.set_callbacks name, callbacks.dup.clear
      end

      # Define sets of events in the object life cycle that support callbacks.
      #
      #   define_callbacks :validate
      #   define_callbacks :initialize, :save, :destroy
      #
      # ===== Options
      #
      # * <tt>:terminator</tt> - Determines when a before filter will halt the
      #   callback chain, preventing following before and around callbacks from
      #   being called and the event from being triggered.
      #   This should be a lambda to be executed.
      #   The current object and the return result of the callback will be called
      #   with the lambda.
      #
      #     define_callbacks :validate, terminator: ->(target, result) { result == false }
      #
      #   In this example, if any before validate callbacks returns +false+,
      #   any successive before and around callback is not executed.
      #   Defaults to +false+, meaning no value halts the chain.
      #
      # * <tt>:skip_after_callbacks_if_terminated</tt> - Determines if after
      #   callbacks should be terminated by the <tt>:terminator</tt> option. By
      #   default after callbacks are executed no matter if callback chain was
      #   terminated or not. This option makes sense only when <tt>:terminator</tt>
      #   option is specified.
      #
      # * <tt>:scope</tt> - Indicates which methods should be executed when an
      #   object is used as a callback.
      #
      #     class Audit
      #       def before(caller)
      #         puts 'Audit: before is called'
      #       end
      #
      #       def before_save(caller)
      #         puts 'Audit: before_save is called'
      #       end
      #     end
      #
      #     class Account
      #       include ActiveSupport::Callbacks
      #
      #       define_callbacks :save
      #       set_callback :save, :before, Audit.new
      #
      #       def save
      #         run_callbacks :save do
      #           puts 'save in main'
      #         end
      #       end
      #     end
      #
      #   In the above case whenever you save an account the method
      #   <tt>Audit#before</tt> will be called. On the other hand
      #
      #     define_callbacks :save, scope: [:kind, :name]
      #
      #   would trigger <tt>Audit#before_save</tt> instead. That's constructed
      #   by calling <tt>#{kind}_#{name}</tt> on the given instance. In this
      #   case "kind" is "before" and "name" is "save". In this context +:kind+
      #   and +:name+ have special meanings: +:kind+ refers to the kind of
      #   callback (before/after/around) and +:name+ refers to the method on
      #   which callbacks are being defined.
      #
      #   A declaration like
      #
      #     define_callbacks :save, scope: [:name]
      #
      #   would call <tt>Audit#save</tt>.
      #
      # NOTE: +method_name+ passed to `define_model_callbacks` must not end with
      # `!`, `?` or `=`.
      def define_callbacks(*names)
        options = names.extract_options!

        names.each do |name|
          class_attribute "_#{name}_callbacks"
          set_callbacks name, CallbackChain.new(name, options)
        end
      end

      protected

      def get_callbacks(name)
        send "_#{name}_callbacks"
      end

      def set_callbacks(name, callbacks)
        send "_#{name}_callbacks=", callbacks
      end
    end
  end
end
module ActiveSupport
  # A typical module looks like this:
  #
  #   module M
  #     def self.included(base)
  #       base.extend ClassMethods
  #       base.class_eval do
  #         scope :disabled, -> { where(disabled: true) }
  #       end
  #     end
  #
  #     module ClassMethods
  #       ...
  #     end
  #   end
  #
  # By using <tt>ActiveSupport::Concern</tt> the above module could instead be
  # written as:
  #
  #   require 'active_support/concern'
  #
  #   module M
  #     extend ActiveSupport::Concern
  #
  #     included do
  #       scope :disabled, -> { where(disabled: true) }
  #     end
  #
  #     class_methods do
  #       ...
  #     end
  #   end
  #
  # Moreover, it gracefully handles module dependencies. Given a +Foo+ module
  # and a +Bar+ module which depends on the former, we would typically write the
  # following:
  #
  #   module Foo
  #     def self.included(base)
  #       base.class_eval do
  #         def self.method_injected_by_foo
  #           ...
  #         end
  #       end
  #     end
  #   end
  #
  #   module Bar
  #     def self.included(base)
  #       base.method_injected_by_foo
  #     end
  #   end
  #
  #   class Host
  #     include Foo # We need to include this dependency for Bar
  #     include Bar # Bar is the module that Host really needs
  #   end
  #
  # But why should +Host+ care about +Bar+'s dependencies, namely +Foo+? We
  # could try to hide these from +Host+ directly including +Foo+ in +Bar+:
  #
  #   module Bar
  #     include Foo
  #     def self.included(base)
  #       base.method_injected_by_foo
  #     end
  #   end
  #
  #   class Host
  #     include Bar
  #   end
  #
  # Unfortunately this won't work, since when +Foo+ is included, its <tt>base</tt>
  # is the +Bar+ module, not the +Host+ class. With <tt>ActiveSupport::Concern</tt>,
  # module dependencies are properly resolved:
  #
  #   require 'active_support/concern'
  #
  #   module Foo
  #     extend ActiveSupport::Concern
  #     included do
  #       def self.method_injected_by_foo
  #         ...
  #       end
  #     end
  #   end
  #
  #   module Bar
  #     extend ActiveSupport::Concern
  #     include Foo
  #
  #     included do
  #       self.method_injected_by_foo
  #     end
  #   end
  #
  #   class Host
  #     include Bar # It works, now Bar takes care of its dependencies
  #   end
  module Concern
    class MultipleIncludedBlocks < StandardError #:nodoc:
      def initialize
        super "Cannot define multiple 'included' blocks for a Concern"
      end
    end

    def self.extended(base) #:nodoc:
      base.instance_variable_set(:@_dependencies, [])
    end

    def append_features(base)
      if base.instance_variable_defined?(:@_dependencies)
        base.instance_variable_get(:@_dependencies) << self
        return false
      else
        return false if base < self
        @_dependencies.each { |dep| base.include(dep) }
        super
        base.extend const_get(:ClassMethods) if const_defined?(:ClassMethods)
        base.class_eval(&@_included_block) if instance_variable_defined?(:@_included_block)
      end
    end

    def included(base = nil, &block)
      if base.nil?
        raise MultipleIncludedBlocks if instance_variable_defined?(:@_included_block)

        @_included_block = block
      else
        super
      end
    end

    def class_methods(&class_methods_module_definition)
      mod = const_defined?(:ClassMethods) ?
        const_get(:ClassMethods) :
        const_set(:ClassMethods, Module.new)

      mod.module_eval(&class_methods_module_definition)
    end
  end
end
require 'thread'
require 'monitor'

module ActiveSupport
  module Concurrency
    class Latch
      def initialize(count = 1)
        @count = count
        @lock = Monitor.new
        @cv = @lock.new_cond
      end

      def release
        @lock.synchronize do
          @count -= 1 if @count > 0
          @cv.broadcast if @count.zero?
        end
      end

      def await
        @lock.synchronize do
          @cv.wait_while { @count > 0 }
        end
      end
    end
  end
end
require 'active_support/concern'
require 'active_support/ordered_options'
require 'active_support/core_ext/array/extract_options'

module ActiveSupport
  # Configurable provides a <tt>config</tt> method to store and retrieve
  # configuration options as an <tt>OrderedHash</tt>.
  module Configurable
    extend ActiveSupport::Concern

    class Configuration < ActiveSupport::InheritableOptions
      def compile_methods!
        self.class.compile_methods!(keys)
      end

      # Compiles reader methods so we don't have to go through method_missing.
      def self.compile_methods!(keys)
        keys.reject { |m| method_defined?(m) }.each do |key|
          class_eval <<-RUBY, __FILE__, __LINE__ + 1
            def #{key}; _get(#{key.inspect}); end
          RUBY
        end
      end
    end

    module ClassMethods
      def config
        @_config ||= if respond_to?(:superclass) && superclass.respond_to?(:config)
          superclass.config.inheritable_copy
        else
          # create a new "anonymous" class that will host the compiled reader methods
          Class.new(Configuration).new
        end
      end

      def configure
        yield config
      end

      # Allows you to add shortcut so that you don't have to refer to attribute
      # through config. Also look at the example for config to contrast.
      #
      # Defines both class and instance config accessors.
      #
      #   class User
      #     include ActiveSupport::Configurable
      #     config_accessor :allowed_access
      #   end
      #
      #   User.allowed_access # => nil
      #   User.allowed_access = false
      #   User.allowed_access # => false
      #
      #   user = User.new
      #   user.allowed_access # => false
      #   user.allowed_access = true
      #   user.allowed_access # => true
      #
      #   User.allowed_access # => false
      #
      # The attribute name must be a valid method name in Ruby.
      #
      #   class User
      #     include ActiveSupport::Configurable
      #     config_accessor :"1_Badname"
      #   end
      #   # => NameError: invalid config attribute name
      #
      # To opt out of the instance writer method, pass <tt>instance_writer: false</tt>.
      # To opt out of the instance reader method, pass <tt>instance_reader: false</tt>.
      #
      #   class User
      #     include ActiveSupport::Configurable
      #     config_accessor :allowed_access, instance_reader: false, instance_writer: false
      #   end
      #
      #   User.allowed_access = false
      #   User.allowed_access # => false
      #
      #   User.new.allowed_access = true # => NoMethodError
      #   User.new.allowed_access        # => NoMethodError
      #
      # Or pass <tt>instance_accessor: false</tt>, to opt out both instance methods.
      #
      #   class User
      #     include ActiveSupport::Configurable
      #     config_accessor :allowed_access, instance_accessor: false
      #   end
      #
      #   User.allowed_access = false
      #   User.allowed_access # => false
      #
      #   User.new.allowed_access = true # => NoMethodError
      #   User.new.allowed_access        # => NoMethodError
      #
      # Also you can pass a block to set up the attribute with a default value.
      #
      #   class User
      #     include ActiveSupport::Configurable
      #     config_accessor :hair_colors do
      #       [:brown, :black, :blonde, :red]
      #     end
      #   end
      #
      #   User.hair_colors # => [:brown, :black, :blonde, :red]
      def config_accessor(*names)
        options = names.extract_options!

        names.each do |name|
          raise NameError.new('invalid config attribute name') unless name =~ /\A[_A-Za-z]\w*\z/

          reader, reader_line = "def #{name}; config.#{name}; end", __LINE__
          writer, writer_line = "def #{name}=(value); config.#{name} = value; end", __LINE__

          singleton_class.class_eval reader, __FILE__, reader_line
          singleton_class.class_eval writer, __FILE__, writer_line

          unless options[:instance_accessor] == false
            class_eval reader, __FILE__, reader_line unless options[:instance_reader] == false
            class_eval writer, __FILE__, writer_line unless options[:instance_writer] == false
          end
          send("#{name}=", yield) if block_given?
        end
      end
      private :config_accessor
    end

    # Reads and writes attributes from a configuration <tt>OrderedHash</tt>.
    #
    #   require 'active_support/configurable'
    #
    #   class User
    #     include ActiveSupport::Configurable
    #   end
    #
    #   user = User.new
    #
    #   user.config.allowed_access = true
    #   user.config.level = 1
    #
    #   user.config.allowed_access # => true
    #   user.config.level          # => 1
    def config
      @_config ||= self.class.config.inheritable_copy
    end
  end
end

DEPRECATED_FILES = ["#{File.dirname(__FILE__)}/core_ext/struct.rb"]
(Dir["#{File.dirname(__FILE__)}/core_ext/*.rb"] - DEPRECATED_FILES).each do |path|
  require path
end
require 'active_support/core_ext/array/wrap'
require 'active_support/core_ext/array/access'
require 'active_support/core_ext/array/conversions'
require 'active_support/core_ext/array/extract_options'
require 'active_support/core_ext/array/grouping'
require 'active_support/core_ext/array/prepend_and_append'
require 'active_support/core_ext/array/inquiry'
class Array
  # Returns the tail of the array from +position+.
  #
  #   %w( a b c d ).from(0)  # => ["a", "b", "c", "d"]
  #   %w( a b c d ).from(2)  # => ["c", "d"]
  #   %w( a b c d ).from(10) # => []
  #   %w().from(0)           # => []
  #   %w( a b c d ).from(-2) # => ["c", "d"]
  #   %w( a b c ).from(-10)  # => []
  def from(position)
    self[position, length] || []
  end

  # Returns the beginning of the array up to +position+.
  #
  #   %w( a b c d ).to(0)  # => ["a"]
  #   %w( a b c d ).to(2)  # => ["a", "b", "c"]
  #   %w( a b c d ).to(10) # => ["a", "b", "c", "d"]
  #   %w().to(0)           # => []
  #   %w( a b c d ).to(-2) # => ["a", "b", "c"]
  #   %w( a b c ).to(-10)  # => []
  def to(position)
    if position >= 0
      take position + 1
    else
      self[0..position]
    end
  end

  # Returns a copy of the Array without the specified elements.
  #
  #   people = ["David", "Rafael", "Aaron", "Todd"]
  #   people.without "Aaron", "Todd"
  #     => ["David", "Rafael"]
  #
  # Note: This is an optimization of `Enumerable#without` that uses `Array#-`
  # instead of `Array#reject` for performance reasons.
  def without(*elements)
    self - elements
  end

  # Equal to <tt>self[1]</tt>.
  #
  #   %w( a b c d e ).second # => "b"
  def second
    self[1]
  end

  # Equal to <tt>self[2]</tt>.
  #
  #   %w( a b c d e ).third # => "c"
  def third
    self[2]
  end

  # Equal to <tt>self[3]</tt>.
  #
  #   %w( a b c d e ).fourth # => "d"
  def fourth
    self[3]
  end

  # Equal to <tt>self[4]</tt>.
  #
  #   %w( a b c d e ).fifth # => "e"
  def fifth
    self[4]
  end

  # Equal to <tt>self[41]</tt>. Also known as accessing "the reddit".
  #
  #   (1..42).to_a.forty_two # => 42
  def forty_two
    self[41]
  end
end
require 'active_support/xml_mini'
require 'active_support/core_ext/hash/keys'
require 'active_support/core_ext/string/inflections'
require 'active_support/core_ext/object/to_param'
require 'active_support/core_ext/object/to_query'

class Array
  # Converts the array to a comma-separated sentence where the last element is
  # joined by the connector word.
  #
  # You can pass the following options to change the default behavior. If you
  # pass an option key that doesn't exist in the list below, it will raise an
  # <tt>ArgumentError</tt>.
  #
  # ==== Options
  #
  # * <tt>:words_connector</tt> - The sign or word used to join the elements
  #   in arrays with two or more elements (default: ", ").
  # * <tt>:two_words_connector</tt> - The sign or word used to join the elements
  #   in arrays with two elements (default: " and ").
  # * <tt>:last_word_connector</tt> - The sign or word used to join the last element
  #   in arrays with three or more elements (default: ", and ").
  # * <tt>:locale</tt> - If +i18n+ is available, you can set a locale and use
  #   the connector options defined on the 'support.array' namespace in the
  #   corresponding dictionary file.
  #
  # ==== Examples
  #
  #   [].to_sentence                      # => ""
  #   ['one'].to_sentence                 # => "one"
  #   ['one', 'two'].to_sentence          # => "one and two"
  #   ['one', 'two', 'three'].to_sentence # => "one, two, and three"
  #
  #   ['one', 'two'].to_sentence(passing: 'invalid option')
  #   # => ArgumentError: Unknown key :passing
  #
  #   ['one', 'two'].to_sentence(two_words_connector: '-')
  #   # => "one-two"
  #
  #   ['one', 'two', 'three'].to_sentence(words_connector: ' or ', last_word_connector: ' or at least ')
  #   # => "one or two or at least three"
  #
  # Using <tt>:locale</tt> option:
  #
  #   # Given this locale dictionary:
  #   #
  #   #   es:
  #   #     support:
  #   #       array:
  #   #         words_connector: " o "
  #   #         two_words_connector: " y "
  #   #         last_word_connector: " o al menos "
  #
  #   ['uno', 'dos'].to_sentence(locale: :es)
  #   # => "uno y dos"
  #
  #   ['uno', 'dos', 'tres'].to_sentence(locale: :es)
  #   # => "uno o dos o al menos tres"
  def to_sentence(options = {})
    options.assert_valid_keys(:words_connector, :two_words_connector, :last_word_connector, :locale)

    default_connectors = {
      :words_connector     => ', ',
      :two_words_connector => ' and ',
      :last_word_connector => ', and '
    }
    if defined?(I18n)
      i18n_connectors = I18n.translate(:'support.array', locale: options[:locale], default: {})
      default_connectors.merge!(i18n_connectors)
    end
    options = default_connectors.merge!(options)

    case length
    when 0
      ''
    when 1
      "#{self[0]}"
    when 2
      "#{self[0]}#{options[:two_words_connector]}#{self[1]}"
    else
      "#{self[0...-1].join(options[:words_connector])}#{options[:last_word_connector]}#{self[-1]}"
    end
  end

  # Extends <tt>Array#to_s</tt> to convert a collection of elements into a
  # comma separated id list if <tt>:db</tt> argument is given as the format.
  #
  #   Blog.all.to_formatted_s(:db) # => "1,2,3"
  def to_formatted_s(format = :default)
    case format
    when :db
      if empty?
        'null'
      else
        collect(&:id).join(',')
      end
    else
      to_default_s
    end
  end
  alias_method :to_default_s, :to_s
  alias_method :to_s, :to_formatted_s

  # Returns a string that represents the array in XML by invoking +to_xml+
  # on each element. Active Record collections delegate their representation
  # in XML to this method.
  #
  # All elements are expected to respond to +to_xml+, if any of them does
  # not then an exception is raised.
  #
  # The root node reflects the class name of the first element in plural
  # if all elements belong to the same type and that's not Hash:
  #
  #   customer.projects.to_xml
  #
  #   <?xml version="1.0" encoding="UTF-8"?>
  #   <projects type="array">
  #     <project>
  #       <amount type="decimal">20000.0</amount>
  #       <customer-id type="integer">1567</customer-id>
  #       <deal-date type="date">2008-04-09</deal-date>
  #       ...
  #     </project>
  #     <project>
  #       <amount type="decimal">57230.0</amount>
  #       <customer-id type="integer">1567</customer-id>
  #       <deal-date type="date">2008-04-15</deal-date>
  #       ...
  #     </project>
  #   </projects>
  #
  # Otherwise the root element is "objects":
  #
  #   [{ foo: 1, bar: 2}, { baz: 3}].to_xml
  #
  #   <?xml version="1.0" encoding="UTF-8"?>
  #   <objects type="array">
  #     <object>
  #       <bar type="integer">2</bar>
  #       <foo type="integer">1</foo>
  #     </object>
  #     <object>
  #       <baz type="integer">3</baz>
  #     </object>
  #   </objects>
  #
  # If the collection is empty the root element is "nil-classes" by default:
  #
  #   [].to_xml
  #
  #   <?xml version="1.0" encoding="UTF-8"?>
  #   <nil-classes type="array"/>
  #
  # To ensure a meaningful root element use the <tt>:root</tt> option:
  #
  #   customer_with_no_projects.projects.to_xml(root: 'projects')
  #
  #   <?xml version="1.0" encoding="UTF-8"?>
  #   <projects type="array"/>
  #
  # By default name of the node for the children of root is <tt>root.singularize</tt>.
  # You can change it with the <tt>:children</tt> option.
  #
  # The +options+ hash is passed downwards:
  #
  #   Message.all.to_xml(skip_types: true)
  #
  #   <?xml version="1.0" encoding="UTF-8"?>
  #   <messages>
  #     <message>
  #       <created-at>2008-03-07T09:58:18+01:00</created-at>
  #       <id>1</id>
  #       <name>1</name>
  #       <updated-at>2008-03-07T09:58:18+01:00</updated-at>
  #       <user-id>1</user-id>
  #     </message>
  #   </messages>
  #
  def to_xml(options = {})
    require 'active_support/builder' unless defined?(Builder)

    options = options.dup
    options[:indent]  ||= 2
    options[:builder] ||= Builder::XmlMarkup.new(indent: options[:indent])
    options[:root]    ||= \
      if first.class != Hash && all? { |e| e.is_a?(first.class) }
        underscored = ActiveSupport::Inflector.underscore(first.class.name)
        ActiveSupport::Inflector.pluralize(underscored).tr('/', '_')
      else
        'objects'
      end

    builder = options[:builder]
    builder.instruct! unless options.delete(:skip_instruct)

    root = ActiveSupport::XmlMini.rename_key(options[:root].to_s, options)
    children = options.delete(:children) || root.singularize
    attributes = options[:skip_types] ? {} : { type: 'array' }

    if empty?
      builder.tag!(root, attributes)
    else
      builder.tag!(root, attributes) do
        each { |value| ActiveSupport::XmlMini.to_tag(children, value, options) }
        yield builder if block_given?
      end
    end
  end
end
class Hash
  # By default, only instances of Hash itself are extractable.
  # Subclasses of Hash may implement this method and return
  # true to declare themselves as extractable. If a Hash
  # is extractable, Array#extract_options! pops it from
  # the Array when it is the last element of the Array.
  def extractable_options?
    instance_of?(Hash)
  end
end

class Array
  # Extracts options from a set of arguments. Removes and returns the last
  # element in the array if it's a hash, otherwise returns a blank hash.
  #
  #   def options(*args)
  #     args.extract_options!
  #   end
  #
  #   options(1, 2)        # => {}
  #   options(1, 2, a: :b) # => {:a=>:b}
  def extract_options!
    if last.is_a?(Hash) && last.extractable_options?
      pop
    else
      {}
    end
  end
end
class Array
  # Splits or iterates over the array in groups of size +number+,
  # padding any remaining slots with +fill_with+ unless it is +false+.
  #
  #   %w(1 2 3 4 5 6 7 8 9 10).in_groups_of(3) {|group| p group}
  #   ["1", "2", "3"]
  #   ["4", "5", "6"]
  #   ["7", "8", "9"]
  #   ["10", nil, nil]
  #
  #   %w(1 2 3 4 5).in_groups_of(2, '&nbsp;') {|group| p group}
  #   ["1", "2"]
  #   ["3", "4"]
  #   ["5", "&nbsp;"]
  #
  #   %w(1 2 3 4 5).in_groups_of(2, false) {|group| p group}
  #   ["1", "2"]
  #   ["3", "4"]
  #   ["5"]
  def in_groups_of(number, fill_with = nil)
    if number.to_i <= 0
      raise ArgumentError,
        "Group size must be a positive integer, was #{number.inspect}"
    end

    if fill_with == false
      collection = self
    else
      # size % number gives how many extra we have;
      # subtracting from number gives how many to add;
      # modulo number ensures we don't add group of just fill.
      padding = (number - size % number) % number
      collection = dup.concat(Array.new(padding, fill_with))
    end

    if block_given?
      collection.each_slice(number) { |slice| yield(slice) }
    else
      collection.each_slice(number).to_a
    end
  end

  # Splits or iterates over the array in +number+ of groups, padding any
  # remaining slots with +fill_with+ unless it is +false+.
  #
  #   %w(1 2 3 4 5 6 7 8 9 10).in_groups(3) {|group| p group}
  #   ["1", "2", "3", "4"]
  #   ["5", "6", "7", nil]
  #   ["8", "9", "10", nil]
  #
  #   %w(1 2 3 4 5 6 7 8 9 10).in_groups(3, '&nbsp;') {|group| p group}
  #   ["1", "2", "3", "4"]
  #   ["5", "6", "7", "&nbsp;"]
  #   ["8", "9", "10", "&nbsp;"]
  #
  #   %w(1 2 3 4 5 6 7).in_groups(3, false) {|group| p group}
  #   ["1", "2", "3"]
  #   ["4", "5"]
  #   ["6", "7"]
  def in_groups(number, fill_with = nil)
    # size.div number gives minor group size;
    # size % number gives how many objects need extra accommodation;
    # each group hold either division or division + 1 items.
    division = size.div number
    modulo = size % number

    # create a new array avoiding dup
    groups = []
    start = 0

    number.times do |index|
      length = division + (modulo > 0 && modulo > index ? 1 : 0)
      groups << last_group = slice(start, length)
      last_group << fill_with if fill_with != false &&
        modulo > 0 && length == division
      start += length
    end

    if block_given?
      groups.each { |g| yield(g) }
    else
      groups
    end
  end

  # Divides the array into one or more subarrays based on a delimiting +value+
  # or the result of an optional block.
  #
  #   [1, 2, 3, 4, 5].split(3)              # => [[1, 2], [4, 5]]
  #   (1..10).to_a.split { |i| i % 3 == 0 } # => [[1, 2], [4, 5], [7, 8], [10]]
  def split(value = nil)
    if block_given?
      inject([[]]) do |results, element|
        if yield(element)
          results << []
        else
          results.last << element
        end

        results
      end
    else
      results, arr = [[]], self.dup
      until arr.empty?
        if (idx = arr.index(value))
          results.last.concat(arr.shift(idx))
          arr.shift
          results << []
        else
          results.last.concat(arr.shift(arr.size))
        end
      end
      results
    end
  end
end
require 'active_support/array_inquirer'

class Array
  # Wraps the array in an +ArrayInquirer+ object, which gives a friendlier way
  # to check its string-like contents.
  #
  #   pets = [:cat, :dog].inquiry
  #
  #   pets.cat?     # => true
  #   pets.ferret?  # => false
  #
  #   pets.any?(:cat, :ferret)  # => true
  #   pets.any?(:ferret, :alligator)  # => false
  def inquiry
    ActiveSupport::ArrayInquirer.new(self)
  end
end
class Array
  # The human way of thinking about adding stuff to the end of a list is with append.
  alias_method :append,  :<<

  # The human way of thinking about adding stuff to the beginning of a list is with prepend.
  alias_method :prepend, :unshift
endclass Array
  # Wraps its argument in an array unless it is already an array (or array-like).
  #
  # Specifically:
  #
  # * If the argument is +nil+ an empty array is returned.
  # * Otherwise, if the argument responds to +to_ary+ it is invoked, and its result returned.
  # * Otherwise, returns an array with the argument as its single element.
  #
  #     Array.wrap(nil)       # => []
  #     Array.wrap([1, 2, 3]) # => [1, 2, 3]
  #     Array.wrap(0)         # => [0]
  #
  # This method is similar in purpose to <tt>Kernel#Array</tt>, but there are some differences:
  #
  # * If the argument responds to +to_ary+ the method is invoked. <tt>Kernel#Array</tt>
  #   moves on to try +to_a+ if the returned value is +nil+, but <tt>Array.wrap</tt> returns
  #   an array with the argument as its single element right away.
  # * If the returned value from +to_ary+ is neither +nil+ nor an +Array+ object, <tt>Kernel#Array</tt>
  #   raises an exception, while <tt>Array.wrap</tt> does not, it just returns the value.
  # * It does not call +to_a+ on the argument, if the argument does not respond to +to_ary+
  #   it returns an array with the argument as its single element.
  #
  # The last point is easily explained with some enumerables:
  #
  #   Array(foo: :bar)      # => [[:foo, :bar]]
  #   Array.wrap(foo: :bar) # => [{:foo=>:bar}]
  #
  # There's also a related idiom that uses the splat operator:
  #
  #   [*object]
  #
  # which returns <tt>[]</tt> for +nil+, but calls to <tt>Array(object)</tt> otherwise.
  #
  # The differences with <tt>Kernel#Array</tt> explained above
  # apply to the rest of <tt>object</tt>s.
  def self.wrap(object)
    if object.nil?
      []
    elsif object.respond_to?(:to_ary)
      object.to_ary || [object]
    else
      [object]
    end
  end
end
require 'benchmark'

class << Benchmark
  # Benchmark realtime in milliseconds.
  #
  #   Benchmark.realtime { User.all }
  #   # => 8.0e-05
  #
  #   Benchmark.ms { User.all }
  #   # => 0.074
  def ms
    1000 * realtime { yield }
  end
end
require 'active_support/core_ext/big_decimal/conversions'
require 'bigdecimal'
require 'bigdecimal/util'

class BigDecimal
  DEFAULT_STRING_FORMAT = 'F'
  alias_method :to_default_s, :to_s

  def to_s(format = nil, options = nil)
    if format.is_a?(Symbol)
      to_formatted_s(format, options || {})
    else
      to_default_s(format || DEFAULT_STRING_FORMAT)
    end
  end
end
require 'active_support/core_ext/class/attribute'
require 'active_support/core_ext/class/delegating_attributes'
require 'active_support/core_ext/class/subclasses'
require 'active_support/core_ext/kernel/singleton_class'
require 'active_support/core_ext/module/remove_method'
require 'active_support/core_ext/array/extract_options'

class Class
  # Declare a class-level attribute whose value is inheritable by subclasses.
  # Subclasses can change their own value and it will not impact parent class.
  #
  #   class Base
  #     class_attribute :setting
  #   end
  #
  #   class Subclass < Base
  #   end
  #
  #   Base.setting = true
  #   Subclass.setting            # => true
  #   Subclass.setting = false
  #   Subclass.setting            # => false
  #   Base.setting                # => true
  #
  # In the above case as long as Subclass does not assign a value to setting
  # by performing <tt>Subclass.setting = _something_ </tt>, <tt>Subclass.setting</tt>
  # would read value assigned to parent class. Once Subclass assigns a value then
  # the value assigned by Subclass would be returned.
  #
  # This matches normal Ruby method inheritance: think of writing an attribute
  # on a subclass as overriding the reader method. However, you need to be aware
  # when using +class_attribute+ with mutable structures as +Array+ or +Hash+.
  # In such cases, you don't want to do changes in places but use setters:
  #
  #   Base.setting = []
  #   Base.setting                # => []
  #   Subclass.setting            # => []
  #
  #   # Appending in child changes both parent and child because it is the same object:
  #   Subclass.setting << :foo
  #   Base.setting               # => [:foo]
  #   Subclass.setting           # => [:foo]
  #
  #   # Use setters to not propagate changes:
  #   Base.setting = []
  #   Subclass.setting += [:foo]
  #   Base.setting               # => []
  #   Subclass.setting           # => [:foo]
  #
  # For convenience, an instance predicate method is defined as well.
  # To skip it, pass <tt>instance_predicate: false</tt>.
  #
  #   Subclass.setting?       # => false
  #
  # Instances may overwrite the class value in the same way:
  #
  #   Base.setting = true
  #   object = Base.new
  #   object.setting          # => true
  #   object.setting = false
  #   object.setting          # => false
  #   Base.setting            # => true
  #
  # To opt out of the instance reader method, pass <tt>instance_reader: false</tt>.
  #
  #   object.setting          # => NoMethodError
  #   object.setting?         # => NoMethodError
  #
  # To opt out of the instance writer method, pass <tt>instance_writer: false</tt>.
  #
  #   object.setting = false  # => NoMethodError
  #
  # To opt out of both instance methods, pass <tt>instance_accessor: false</tt>.
  def class_attribute(*attrs)
    options = attrs.extract_options!
    instance_reader = options.fetch(:instance_accessor, true) && options.fetch(:instance_reader, true)
    instance_writer = options.fetch(:instance_accessor, true) && options.fetch(:instance_writer, true)
    instance_predicate = options.fetch(:instance_predicate, true)

    attrs.each do |name|
      define_singleton_method(name) { nil }
      define_singleton_method("#{name}?") { !!public_send(name) } if instance_predicate

      ivar = "@#{name}"

      define_singleton_method("#{name}=") do |val|
        singleton_class.class_eval do
          remove_possible_method(name)
          define_method(name) { val }
        end

        if singleton_class?
          class_eval do
            remove_possible_method(name)
            define_method(name) do
              if instance_variable_defined? ivar
                instance_variable_get ivar
              else
                singleton_class.send name
              end
            end
          end
        end
        val
      end

      if instance_reader
        remove_possible_method name
        define_method(name) do
          if instance_variable_defined?(ivar)
            instance_variable_get ivar
          else
            self.class.public_send name
          end
        end
        define_method("#{name}?") { !!public_send(name) } if instance_predicate
      end

      attr_writer name if instance_writer
    end
  end
end
# cattr_* became mattr_* aliases in 7dfbd91b0780fbd6a1dd9bfbc176e10894871d2d,
# but we keep this around for libraries that directly require it knowing they
# want cattr_*. No need to deprecate.
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/core_ext/kernel/singleton_class'
require 'active_support/core_ext/module/remove_method'
require 'active_support/core_ext/module/deprecation'


class Class
  def superclass_delegating_accessor(name, options = {})
    # Create private _name and _name= methods that can still be used if the public
    # methods are overridden.
    _superclass_delegating_accessor("_#{name}", options)

    # Generate the public methods name, name=, and name?.
    # These methods dispatch to the private _name, and _name= methods, making them
    # overridable.
    singleton_class.send(:define_method, name) { send("_#{name}") }
    singleton_class.send(:define_method, "#{name}?") { !!send("_#{name}") }
    singleton_class.send(:define_method, "#{name}=") { |value| send("_#{name}=", value) }

    # If an instance_reader is needed, generate public instance methods name and name?.
    if options[:instance_reader] != false
      define_method(name) { send("_#{name}") }
      define_method("#{name}?") { !!send("#{name}") }
    end
  end

  deprecate superclass_delegating_accessor: :class_attribute

  private
    # Take the object being set and store it in a method. This gives us automatic
    # inheritance behavior, without having to store the object in an instance
    # variable and look up the superclass chain manually.
    def _stash_object_in_method(object, method, instance_reader = true)
      singleton_class.remove_possible_method(method)
      singleton_class.send(:define_method, method) { object }
      remove_possible_method(method)
      define_method(method) { object } if instance_reader
    end

    def _superclass_delegating_accessor(name, options = {})
      singleton_class.send(:define_method, "#{name}=") do |value|
        _stash_object_in_method(value, name, options[:instance_reader] != false)
      end
      send("#{name}=", nil)
    end
end
require 'active_support/core_ext/module/anonymous'
require 'active_support/core_ext/module/reachable'

class Class
  begin
    ObjectSpace.each_object(Class.new) {}

    def descendants # :nodoc:
      descendants = []
      ObjectSpace.each_object(singleton_class) do |k|
        descendants.unshift k unless k == self
      end
      descendants
    end
  rescue StandardError # JRuby
    def descendants # :nodoc:
      descendants = []
      ObjectSpace.each_object(Class) do |k|
        descendants.unshift k if k < self
      end
      descendants.uniq!
      descendants
    end
  end

  # Returns an array with the direct children of +self+.
  #
  #   Integer.subclasses # => [Fixnum, Bignum]
  #
  #   class Foo; end
  #   class Bar < Foo; end
  #   class Baz < Bar; end
  #
  #   Foo.subclasses # => [Bar]
  def subclasses
    subclasses, chain = [], descendants
    chain.each do |k|
      subclasses << k unless chain.any? { |c| c > k }
    end
    subclasses
  end
end
require 'active_support/core_ext/date/acts_like'
require 'active_support/core_ext/date/calculations'
require 'active_support/core_ext/date/conversions'
require 'active_support/core_ext/date/zones'

require 'active_support/core_ext/object/acts_like'

class Date
  # Duck-types as a Date-like class. See Object#acts_like?.
  def acts_like_date?
    true
  end
end
require 'date'
require 'active_support/duration'
require 'active_support/core_ext/object/acts_like'
require 'active_support/core_ext/date/zones'
require 'active_support/core_ext/time/zones'
require 'active_support/core_ext/date_and_time/calculations'

class Date
  include DateAndTime::Calculations

  class << self
    attr_accessor :beginning_of_week_default

    # Returns the week start (e.g. :monday) for the current request, if this has been set (via Date.beginning_of_week=).
    # If <tt>Date.beginning_of_week</tt> has not been set for the current request, returns the week start specified in <tt>config.beginning_of_week</tt>.
    # If no config.beginning_of_week was specified, returns :monday.
    def beginning_of_week
      Thread.current[:beginning_of_week] || beginning_of_week_default || :monday
    end

    # Sets <tt>Date.beginning_of_week</tt> to a week start (e.g. :monday) for current request/thread.
    #
    # This method accepts any of the following day symbols:
    # :monday, :tuesday, :wednesday, :thursday, :friday, :saturday, :sunday
    def beginning_of_week=(week_start)
      Thread.current[:beginning_of_week] = find_beginning_of_week!(week_start)
    end

    # Returns week start day symbol (e.g. :monday), or raises an ArgumentError for invalid day symbol.
    def find_beginning_of_week!(week_start)
      raise ArgumentError, "Invalid beginning of week: #{week_start}" unless ::Date::DAYS_INTO_WEEK.key?(week_start)
      week_start
    end

    # Returns a new Date representing the date 1 day ago (i.e. yesterday's date).
    def yesterday
      ::Date.current.yesterday
    end

    # Returns a new Date representing the date 1 day after today (i.e. tomorrow's date).
    def tomorrow
      ::Date.current.tomorrow
    end

    # Returns Time.zone.today when <tt>Time.zone</tt> or <tt>config.time_zone</tt> are set, otherwise just returns Date.today.
    def current
      ::Time.zone ? ::Time.zone.today : ::Date.today
    end
  end

  # Converts Date to a Time (or DateTime if necessary) with the time portion set to the beginning of the day (0:00)
  # and then subtracts the specified number of seconds.
  def ago(seconds)
    in_time_zone.since(-seconds)
  end

  # Converts Date to a Time (or DateTime if necessary) with the time portion set to the beginning of the day (0:00)
  # and then adds the specified number of seconds
  def since(seconds)
    in_time_zone.since(seconds)
  end
  alias :in :since

  # Converts Date to a Time (or DateTime if necessary) with the time portion set to the beginning of the day (0:00)
  def beginning_of_day
    in_time_zone
  end
  alias :midnight :beginning_of_day
  alias :at_midnight :beginning_of_day
  alias :at_beginning_of_day :beginning_of_day

  # Converts Date to a Time (or DateTime if necessary) with the time portion set to the middle of the day (12:00)
  def middle_of_day
    in_time_zone.middle_of_day
  end
  alias :midday :middle_of_day
  alias :noon :middle_of_day
  alias :at_midday :middle_of_day
  alias :at_noon :middle_of_day
  alias :at_middle_of_day :middle_of_day

  # Converts Date to a Time (or DateTime if necessary) with the time portion set to the end of the day (23:59:59)
  def end_of_day
    in_time_zone.end_of_day
  end
  alias :at_end_of_day :end_of_day

  def plus_with_duration(other) #:nodoc:
    if ActiveSupport::Duration === other
      other.since(self)
    else
      plus_without_duration(other)
    end
  end
  alias_method :plus_without_duration, :+
  alias_method :+, :plus_with_duration

  def minus_with_duration(other) #:nodoc:
    if ActiveSupport::Duration === other
      plus_with_duration(-other)
    else
      minus_without_duration(other)
    end
  end
  alias_method :minus_without_duration, :-
  alias_method :-, :minus_with_duration

  # Provides precise Date calculations for years, months, and days. The +options+ parameter takes a hash with
  # any of these keys: <tt>:years</tt>, <tt>:months</tt>, <tt>:weeks</tt>, <tt>:days</tt>.
  def advance(options)
    options = options.dup
    d = self
    d = d >> options.delete(:years) * 12 if options[:years]
    d = d >> options.delete(:months)     if options[:months]
    d = d +  options.delete(:weeks) * 7  if options[:weeks]
    d = d +  options.delete(:days)       if options[:days]
    d
  end

  # Returns a new Date where one or more of the elements have been changed according to the +options+ parameter.
  # The +options+ parameter is a hash with a combination of these keys: <tt>:year</tt>, <tt>:month</tt>, <tt>:day</tt>.
  #
  #   Date.new(2007, 5, 12).change(day: 1)               # => Date.new(2007, 5, 1)
  #   Date.new(2007, 5, 12).change(year: 2005, month: 1) # => Date.new(2005, 1, 12)
  def change(options)
    ::Date.new(
      options.fetch(:year, year),
      options.fetch(:month, month),
      options.fetch(:day, day)
    )
  end
  
  # Allow Date to be compared with Time by converting to DateTime and relying on the <=> from there.
  def compare_with_coercion(other)
    if other.is_a?(Time)
      self.to_datetime <=> other
    else
      compare_without_coercion(other)
    end
  end
  alias_method :compare_without_coercion, :<=>
  alias_method :<=>, :compare_with_coercion
end
require 'date'
require 'active_support/inflector/methods'
require 'active_support/core_ext/date/zones'
require 'active_support/core_ext/module/remove_method'

class Date
  DATE_FORMATS = {
    :short        => '%e %b',
    :long         => '%B %e, %Y',
    :db           => '%Y-%m-%d',
    :number       => '%Y%m%d',
    :long_ordinal => lambda { |date|
      day_format = ActiveSupport::Inflector.ordinalize(date.day)
      date.strftime("%B #{day_format}, %Y") # => "April 25th, 2007"
    },
    :rfc822       => '%e %b %Y',
    :iso8601      => lambda { |date| date.iso8601 }
  }

  # Ruby 1.9 has Date#to_time which converts to localtime only.
  remove_method :to_time

  # Ruby 1.9 has Date#xmlschema which converts to a string without the time
  # component. This removal may generate an issue on FreeBSD, that's why we
  # need to use remove_possible_method here
  remove_possible_method :xmlschema

  # Convert to a formatted string. See DATE_FORMATS for predefined formats.
  #
  # This method is aliased to <tt>to_s</tt>.
  #
  #   date = Date.new(2007, 11, 10)       # => Sat, 10 Nov 2007
  #
  #   date.to_formatted_s(:db)            # => "2007-11-10"
  #   date.to_s(:db)                      # => "2007-11-10"
  #
  #   date.to_formatted_s(:short)         # => "10 Nov"
  #   date.to_formatted_s(:number)        # => "20071110"
  #   date.to_formatted_s(:long)          # => "November 10, 2007"
  #   date.to_formatted_s(:long_ordinal)  # => "November 10th, 2007"
  #   date.to_formatted_s(:rfc822)        # => "10 Nov 2007"
  #   date.to_formatted_s(:iso8601)       # => "2007-11-10"
  #
  # == Adding your own date formats to to_formatted_s
  # You can add your own formats to the Date::DATE_FORMATS hash.
  # Use the format name as the hash key and either a strftime string
  # or Proc instance that takes a date argument as the value.
  #
  #   # config/initializers/date_formats.rb
  #   Date::DATE_FORMATS[:month_and_year] = '%B %Y'
  #   Date::DATE_FORMATS[:short_ordinal] = ->(date) { date.strftime("%B #{date.day.ordinalize}") }
  def to_formatted_s(format = :default)
    if formatter = DATE_FORMATS[format]
      if formatter.respond_to?(:call)
        formatter.call(self).to_s
      else
        strftime(formatter)
      end
    else
      to_default_s
    end
  end
  alias_method :to_default_s, :to_s
  alias_method :to_s, :to_formatted_s

  # Overrides the default inspect method with a human readable one, e.g., "Mon, 21 Feb 2005"
  def readable_inspect
    strftime('%a, %d %b %Y')
  end
  alias_method :default_inspect, :inspect
  alias_method :inspect, :readable_inspect

  # Converts a Date instance to a Time, where the time is set to the beginning of the day.
  # The timezone can be either :local or :utc (default :local).
  #
  #   date = Date.new(2007, 11, 10)  # => Sat, 10 Nov 2007
  #
  #   date.to_time                   # => Sat Nov 10 00:00:00 0800 2007
  #   date.to_time(:local)           # => Sat Nov 10 00:00:00 0800 2007
  #
  #   date.to_time(:utc)             # => Sat Nov 10 00:00:00 UTC 2007
  def to_time(form = :local)
    ::Time.send(form, year, month, day)
  end

  # Returns a string which represents the time in used time zone as DateTime
  # defined by XML Schema:
  #
  #   date = Date.new(2015, 05, 23)  # => Sat, 23 May 2015
  #   date.xmlschema                 # => "2015-05-23T00:00:00+04:00"
  def xmlschema
    in_time_zone.xmlschema
  end
end
require 'date'
require 'active_support/core_ext/date_and_time/zones'

class Date
  include DateAndTime::Zones
end
module DateAndTime
  module Calculations
    DAYS_INTO_WEEK = {
      :monday    => 0,
      :tuesday   => 1,
      :wednesday => 2,
      :thursday  => 3,
      :friday    => 4,
      :saturday  => 5,
      :sunday    => 6
    }
    WEEKEND_DAYS = [ 6, 0 ]

    # Returns a new date/time representing yesterday.
    def yesterday
      advance(days: -1)
    end

    # Returns a new date/time representing the previous day.
    def prev_day
      advance(days: -1)
    end

    # Returns a new date/time representing tomorrow.
    def tomorrow
      advance(days: 1)
    end

    # Returns a new date/time representing the next day.
    def next_day
      advance(days: 1)
    end

    # Returns true if the date/time is today.
    def today?
      to_date == ::Date.current
    end

    # Returns true if the date/time is in the past.
    def past?
      self < self.class.current
    end

    # Returns true if the date/time is in the future.
    def future?
      self > self.class.current
    end

    # Returns true if the date/time falls on a Saturday or Sunday.
    def on_weekend?
      WEEKEND_DAYS.include?(wday)
    end

    # Returns a new date/time the specified number of days ago.
    def days_ago(days)
      advance(:days => -days)
    end

    # Returns a new date/time the specified number of days in the future.
    def days_since(days)
      advance(:days => days)
    end

    # Returns a new date/time the specified number of weeks ago.
    def weeks_ago(weeks)
      advance(:weeks => -weeks)
    end

    # Returns a new date/time the specified number of weeks in the future.
    def weeks_since(weeks)
      advance(:weeks => weeks)
    end

    # Returns a new date/time the specified number of months ago.
    def months_ago(months)
      advance(:months => -months)
    end

    # Returns a new date/time the specified number of months in the future.
    def months_since(months)
      advance(:months => months)
    end

    # Returns a new date/time the specified number of years ago.
    def years_ago(years)
      advance(:years => -years)
    end

    # Returns a new date/time the specified number of years in the future.
    def years_since(years)
      advance(:years => years)
    end

    # Returns a new date/time at the start of the month.
    # DateTime objects will have a time set to 0:00.
    def beginning_of_month
      first_hour(change(:day => 1))
    end
    alias :at_beginning_of_month :beginning_of_month

    # Returns a new date/time at the start of the quarter.
    # Example: 1st January, 1st July, 1st October.
    # DateTime objects will have a time set to 0:00.
    def beginning_of_quarter
      first_quarter_month = [10, 7, 4, 1].detect { |m| m <= month }
      beginning_of_month.change(:month => first_quarter_month)
    end
    alias :at_beginning_of_quarter :beginning_of_quarter

    # Returns a new date/time at the end of the quarter.
    # Example: 31st March, 30th June, 30th September.
    # DateTime objects will have a time set to 23:59:59.
    def end_of_quarter
      last_quarter_month = [3, 6, 9, 12].detect { |m| m >= month }
      beginning_of_month.change(:month => last_quarter_month).end_of_month
    end
    alias :at_end_of_quarter :end_of_quarter

    # Return a new date/time at the beginning of the year.
    # Example: 1st January.
    # DateTime objects will have a time set to 0:00.
    def beginning_of_year
      change(:month => 1).beginning_of_month
    end
    alias :at_beginning_of_year :beginning_of_year

    # Returns a new date/time representing the given day in the next week.
    #
    #   today = Date.today # => Thu, 07 May 2015
    #   today.next_week    # => Mon, 11 May 2015
    #
    # The +given_day_in_next_week+ defaults to the beginning of the week
    # which is determined by +Date.beginning_of_week+ or +config.beginning_of_week+
    # when set.
    #
    #   today = Date.today       # => Thu, 07 May 2015
    #   today.next_week(:friday) # => Fri, 15 May 2015
    #
    # +DateTime+ objects have their time set to 0:00 unless +same_time+ is true.
    #
    #   now = Time.current # => Thu, 07 May 2015 13:31:16 UTC +00:00
    #   now.next_week      # => Mon, 11 May 2015 00:00:00 UTC +00:00
    def next_week(given_day_in_next_week = Date.beginning_of_week, same_time: false)
      result = first_hour(weeks_since(1).beginning_of_week.days_since(days_span(given_day_in_next_week)))
      same_time ? copy_time_to(result) : result
    end

    # Returns a new date/time representing the next weekday.
    def next_weekday
      if next_day.on_weekend?
        next_week(:monday, same_time: true)
      else
        next_day
      end
    end

    # Short-hand for months_since(1).
    def next_month
      months_since(1)
    end

    # Short-hand for months_since(3)
    def next_quarter
      months_since(3)
    end

    # Short-hand for years_since(1).
    def next_year
      years_since(1)
    end

    # Returns a new date/time representing the given day in the previous week.
    # Week is assumed to start on +start_day+, default is
    # +Date.beginning_of_week+ or +config.beginning_of_week+ when set.
    # DateTime objects have their time set to 0:00 unless +same_time+ is true.
    def prev_week(start_day = Date.beginning_of_week, same_time: false)
      result = first_hour(weeks_ago(1).beginning_of_week.days_since(days_span(start_day)))
      same_time ? copy_time_to(result) : result
    end
    alias_method :last_week, :prev_week

    # Returns a new date/time representing the previous weekday.
    def prev_weekday
      if prev_day.on_weekend?
        copy_time_to(beginning_of_week(:friday))
      else
        prev_day
      end
    end
    alias_method :last_weekday, :prev_weekday

    # Short-hand for months_ago(1).
    def prev_month
      months_ago(1)
    end
    alias_method :last_month, :prev_month

    # Short-hand for months_ago(3).
    def prev_quarter
      months_ago(3)
    end
    alias_method :last_quarter, :prev_quarter

    # Short-hand for years_ago(1).
    def prev_year
      years_ago(1)
    end
    alias_method :last_year, :prev_year

    # Returns the number of days to the start of the week on the given day.
    # Week is assumed to start on +start_day+, default is
    # +Date.beginning_of_week+ or +config.beginning_of_week+ when set.
    def days_to_week_start(start_day = Date.beginning_of_week)
      start_day_number = DAYS_INTO_WEEK[start_day]
      current_day_number = wday != 0 ? wday - 1 : 6
      (current_day_number - start_day_number) % 7
    end

    # Returns a new date/time representing the start of this week on the given day.
    # Week is assumed to start on +start_day+, default is
    # +Date.beginning_of_week+ or +config.beginning_of_week+ when set.
    # +DateTime+ objects have their time set to 0:00.
    def beginning_of_week(start_day = Date.beginning_of_week)
      result = days_ago(days_to_week_start(start_day))
      acts_like?(:time) ? result.midnight : result
    end
    alias :at_beginning_of_week :beginning_of_week

    # Returns Monday of this week assuming that week starts on Monday.
    # +DateTime+ objects have their time set to 0:00.
    def monday
      beginning_of_week(:monday)
    end

    # Returns a new date/time representing the end of this week on the given day.
    # Week is assumed to start on +start_day+, default is
    # +Date.beginning_of_week+ or +config.beginning_of_week+ when set.
    # DateTime objects have their time set to 23:59:59.
    def end_of_week(start_day = Date.beginning_of_week)
      last_hour(days_since(6 - days_to_week_start(start_day)))
    end
    alias :at_end_of_week :end_of_week

    # Returns Sunday of this week assuming that week starts on Monday.
    # +DateTime+ objects have their time set to 23:59:59.
    def sunday
      end_of_week(:monday)
    end

    # Returns a new date/time representing the end of the month.
    # DateTime objects will have a time set to 23:59:59.
    def end_of_month
      last_day = ::Time.days_in_month(month, year)
      last_hour(days_since(last_day - day))
    end
    alias :at_end_of_month :end_of_month

    # Returns a new date/time representing the end of the year.
    # DateTime objects will have a time set to 23:59:59.
    def end_of_year
      change(:month => 12).end_of_month
    end
    alias :at_end_of_year :end_of_year

    # Returns a Range representing the whole week of the current date/time.
    # Week starts on start_day, default is <tt>Date.week_start</tt> or <tt>config.week_start</tt> when set.
    def all_week(start_day = Date.beginning_of_week)
      beginning_of_week(start_day)..end_of_week(start_day)
    end

    # Returns a Range representing the whole month of the current date/time.
    def all_month
      beginning_of_month..end_of_month
    end

    # Returns a Range representing the whole quarter of the current date/time.
    def all_quarter
      beginning_of_quarter..end_of_quarter
    end

    # Returns a Range representing the whole year of the current date/time.
    def all_year
      beginning_of_year..end_of_year
    end

    private
      def first_hour(date_or_time)
        date_or_time.acts_like?(:time) ? date_or_time.beginning_of_day : date_or_time
      end

      def last_hour(date_or_time)
        date_or_time.acts_like?(:time) ? date_or_time.end_of_day : date_or_time
      end

      def days_span(day)
        (DAYS_INTO_WEEK[day] - DAYS_INTO_WEEK[Date.beginning_of_week]) % 7
      end

      def copy_time_to(other)
        other.change(hour: hour, min: min, sec: sec, usec: try(:usec))
      end
  end
end
module DateAndTime
  module Zones
    # Returns the simultaneous time in <tt>Time.zone</tt> if a zone is given or
    # if Time.zone_default is set. Otherwise, it returns the current time.
    #
    #   Time.zone = 'Hawaii'        # => 'Hawaii'
    #   DateTime.utc(2000).in_time_zone # => Fri, 31 Dec 1999 14:00:00 HST -10:00
    #   Date.new(2000).in_time_zone  # => Sat, 01 Jan 2000 00:00:00 HST -10:00
    #
    # This method is similar to Time#localtime, except that it uses <tt>Time.zone</tt> as the local zone
    # instead of the operating system's time zone.
    #
    # You can also pass in a TimeZone instance or string that identifies a TimeZone as an argument,
    # and the conversion will be based on that zone instead of <tt>Time.zone</tt>.
    #
    #   Time.utc(2000).in_time_zone('Alaska') # => Fri, 31 Dec 1999 15:00:00 AKST -09:00
    #   DateTime.utc(2000).in_time_zone('Alaska') # => Fri, 31 Dec 1999 15:00:00 AKST -09:00
    #   Date.new(2000).in_time_zone('Alaska')  # => Sat, 01 Jan 2000 00:00:00 AKST -09:00
    def in_time_zone(zone = ::Time.zone)
      time_zone = ::Time.find_zone! zone
      time = acts_like?(:time) ? self : nil

      if time_zone
        time_with_zone(time, time_zone)
      else
        time || self.to_time
      end
    end

    private

    def time_with_zone(time, zone)
      if time
        ActiveSupport::TimeWithZone.new(time.utc? ? time : time.getutc, zone)
      else
        ActiveSupport::TimeWithZone.new(nil, zone, to_time(:utc))
      end
    end
  end
end

require 'active_support/core_ext/date_time/acts_like'
require 'active_support/core_ext/date_time/calculations'
require 'active_support/core_ext/date_time/conversions'
require 'active_support/core_ext/date_time/zones'
require 'date'
require 'active_support/core_ext/object/acts_like'

class DateTime
  # Duck-types as a Date-like class. See Object#acts_like?.
  def acts_like_date?
    true
  end

  # Duck-types as a Time-like class. See Object#acts_like?.
  def acts_like_time?
    true
  end
end
require 'date'

class DateTime
  class << self
    # Returns <tt>Time.zone.now.to_datetime</tt> when <tt>Time.zone</tt> or
    # <tt>config.time_zone</tt> are set, otherwise returns
    # <tt>Time.now.to_datetime</tt>.
    def current
      ::Time.zone ? ::Time.zone.now.to_datetime : ::Time.now.to_datetime
    end
  end

  # Returns the number of seconds since 00:00:00.
  #
  #   DateTime.new(2012, 8, 29,  0,  0,  0).seconds_since_midnight # => 0
  #   DateTime.new(2012, 8, 29, 12, 34, 56).seconds_since_midnight # => 45296
  #   DateTime.new(2012, 8, 29, 23, 59, 59).seconds_since_midnight # => 86399
  def seconds_since_midnight
    sec + (min * 60) + (hour * 3600)
  end

  # Returns the number of seconds until 23:59:59.
  #
  #   DateTime.new(2012, 8, 29,  0,  0,  0).seconds_until_end_of_day # => 86399
  #   DateTime.new(2012, 8, 29, 12, 34, 56).seconds_until_end_of_day # => 41103
  #   DateTime.new(2012, 8, 29, 23, 59, 59).seconds_until_end_of_day # => 0
  def seconds_until_end_of_day
    end_of_day.to_i - to_i
  end

  # Returns a new DateTime where one or more of the elements have been changed
  # according to the +options+ parameter. The time options (<tt>:hour</tt>,
  # <tt>:min</tt>, <tt>:sec</tt>) reset cascadingly, so if only the hour is
  # passed, then minute and sec is set to 0. If the hour and minute is passed,
  # then sec is set to 0. The +options+ parameter takes a hash with any of these
  # keys: <tt>:year</tt>, <tt>:month</tt>, <tt>:day</tt>, <tt>:hour</tt>,
  # <tt>:min</tt>, <tt>:sec</tt>, <tt>:offset</tt>, <tt>:start</tt>.
  #
  #   DateTime.new(2012, 8, 29, 22, 35, 0).change(day: 1)              # => DateTime.new(2012, 8, 1, 22, 35, 0)
  #   DateTime.new(2012, 8, 29, 22, 35, 0).change(year: 1981, day: 1)  # => DateTime.new(1981, 8, 1, 22, 35, 0)
  #   DateTime.new(2012, 8, 29, 22, 35, 0).change(year: 1981, hour: 0) # => DateTime.new(1981, 8, 29, 0, 0, 0)
  def change(options)
    ::DateTime.civil(
      options.fetch(:year, year),
      options.fetch(:month, month),
      options.fetch(:day, day),
      options.fetch(:hour, hour),
      options.fetch(:min, options[:hour] ? 0 : min),
      options.fetch(:sec, (options[:hour] || options[:min]) ? 0 : sec + sec_fraction),
      options.fetch(:offset, offset),
      options.fetch(:start, start)
    )
  end

  # Uses Date to provide precise Time calculations for years, months, and days.
  # The +options+ parameter takes a hash with any of these keys: <tt>:years</tt>,
  # <tt>:months</tt>, <tt>:weeks</tt>, <tt>:days</tt>, <tt>:hours</tt>,
  # <tt>:minutes</tt>, <tt>:seconds</tt>.
  def advance(options)
    unless options[:weeks].nil?
      options[:weeks], partial_weeks = options[:weeks].divmod(1)
      options[:days] = options.fetch(:days, 0) + 7 * partial_weeks
    end

    unless options[:days].nil?
      options[:days], partial_days = options[:days].divmod(1)
      options[:hours] = options.fetch(:hours, 0) + 24 * partial_days
    end

    d = to_date.advance(options)
    datetime_advanced_by_date = change(:year => d.year, :month => d.month, :day => d.day)
    seconds_to_advance = \
      options.fetch(:seconds, 0) +
      options.fetch(:minutes, 0) * 60 +
      options.fetch(:hours, 0) * 3600

    if seconds_to_advance.zero?
      datetime_advanced_by_date
    else
      datetime_advanced_by_date.since(seconds_to_advance)
    end
  end

  # Returns a new DateTime representing the time a number of seconds ago.
  # Do not use this method in combination with x.months, use months_ago instead!
  def ago(seconds)
    since(-seconds)
  end

  # Returns a new DateTime representing the time a number of seconds since the
  # instance time. Do not use this method in combination with x.months, use
  # months_since instead!
  def since(seconds)
    self + Rational(seconds.round, 86400)
  end
  alias :in :since

  # Returns a new DateTime representing the start of the day (0:00).
  def beginning_of_day
    change(:hour => 0)
  end
  alias :midnight :beginning_of_day
  alias :at_midnight :beginning_of_day
  alias :at_beginning_of_day :beginning_of_day

  # Returns a new DateTime representing the middle of the day (12:00)
  def middle_of_day
    change(:hour => 12)
  end
  alias :midday :middle_of_day
  alias :noon :middle_of_day
  alias :at_midday :middle_of_day
  alias :at_noon :middle_of_day
  alias :at_middle_of_day :middle_of_day

  # Returns a new DateTime representing the end of the day (23:59:59).
  def end_of_day
    change(:hour => 23, :min => 59, :sec => 59)
  end
  alias :at_end_of_day :end_of_day

  # Returns a new DateTime representing the start of the hour (hh:00:00).
  def beginning_of_hour
    change(:min => 0)
  end
  alias :at_beginning_of_hour :beginning_of_hour

  # Returns a new DateTime representing the end of the hour (hh:59:59).
  def end_of_hour
    change(:min => 59, :sec => 59)
  end
  alias :at_end_of_hour :end_of_hour

  # Returns a new DateTime representing the start of the minute (hh:mm:00).
  def beginning_of_minute
    change(:sec => 0)
  end
  alias :at_beginning_of_minute :beginning_of_minute

  # Returns a new DateTime representing the end of the minute (hh:mm:59).
  def end_of_minute
    change(:sec => 59)
  end
  alias :at_end_of_minute :end_of_minute

  # Adjusts DateTime to UTC by adding its offset value; offset is set to 0.
  #
  #   DateTime.civil(2005, 2, 21, 10, 11, 12, Rational(-6, 24))     # => Mon, 21 Feb 2005 10:11:12 -0600
  #   DateTime.civil(2005, 2, 21, 10, 11, 12, Rational(-6, 24)).utc # => Mon, 21 Feb 2005 16:11:12 +0000
  def utc
    new_offset(0)
  end
  alias_method :getutc, :utc

  # Returns +true+ if <tt>offset == 0</tt>.
  def utc?
    offset == 0
  end

  # Returns the offset value in seconds.
  def utc_offset
    (offset * 86400).to_i
  end

  # Layers additional behavior on DateTime#<=> so that Time and
  # ActiveSupport::TimeWithZone instances can be compared with a DateTime.
  def <=>(other)
    if other.kind_of?(Infinity)
      super
    elsif other.respond_to? :to_datetime
      super other.to_datetime
    else
      nil
    end
  end

end
require 'date'
require 'active_support/inflector/methods'
require 'active_support/core_ext/time/conversions'
require 'active_support/core_ext/date_time/calculations'
require 'active_support/values/time_zone'

class DateTime
  # Convert to a formatted string. See Time::DATE_FORMATS for predefined formats.
  #
  # This method is aliased to <tt>to_s</tt>.
  #
  # === Examples
  #   datetime = DateTime.civil(2007, 12, 4, 0, 0, 0, 0)   # => Tue, 04 Dec 2007 00:00:00 +0000
  #
  #   datetime.to_formatted_s(:db)            # => "2007-12-04 00:00:00"
  #   datetime.to_s(:db)                      # => "2007-12-04 00:00:00"
  #   datetime.to_s(:number)                  # => "20071204000000"
  #   datetime.to_formatted_s(:short)         # => "04 Dec 00:00"
  #   datetime.to_formatted_s(:long)          # => "December 04, 2007 00:00"
  #   datetime.to_formatted_s(:long_ordinal)  # => "December 4th, 2007 00:00"
  #   datetime.to_formatted_s(:rfc822)        # => "Tue, 04 Dec 2007 00:00:00 +0000"
  #   datetime.to_formatted_s(:iso8601)       # => "2007-12-04T00:00:00+00:00"
  #
  # == Adding your own datetime formats to to_formatted_s
  # DateTime formats are shared with Time. You can add your own to the
  # Time::DATE_FORMATS hash. Use the format name as the hash key and
  # either a strftime string or Proc instance that takes a time or
  # datetime argument as the value.
  #
  #   # config/initializers/time_formats.rb
  #   Time::DATE_FORMATS[:month_and_year] = '%B %Y'
  #   Time::DATE_FORMATS[:short_ordinal] = lambda { |time| time.strftime("%B #{time.day.ordinalize}") }
  def to_formatted_s(format = :default)
    if formatter = ::Time::DATE_FORMATS[format]
      formatter.respond_to?(:call) ? formatter.call(self).to_s : strftime(formatter)
    else
      to_default_s
    end
  end
  alias_method :to_default_s, :to_s if instance_methods(false).include?(:to_s)
  alias_method :to_s, :to_formatted_s

  #
  #   datetime = DateTime.civil(2000, 1, 1, 0, 0, 0, Rational(-6, 24))
  #   datetime.formatted_offset         # => "-06:00"
  #   datetime.formatted_offset(false)  # => "-0600"
  def formatted_offset(colon = true, alternate_utc_string = nil)
    utc? && alternate_utc_string || ActiveSupport::TimeZone.seconds_to_utc_offset(utc_offset, colon)
  end

  # Overrides the default inspect method with a human readable one, e.g., "Mon, 21 Feb 2005 14:30:00 +0000".
  def readable_inspect
    to_s(:rfc822)
  end
  alias_method :default_inspect, :inspect
  alias_method :inspect, :readable_inspect

  # Returns DateTime with local offset for given year if format is local else
  # offset is zero.
  #
  #   DateTime.civil_from_format :local, 2012
  #   # => Sun, 01 Jan 2012 00:00:00 +0300
  #   DateTime.civil_from_format :local, 2012, 12, 17
  #   # => Mon, 17 Dec 2012 00:00:00 +0000
  def self.civil_from_format(utc_or_local, year, month=1, day=1, hour=0, min=0, sec=0)
    if utc_or_local.to_sym == :local
      offset = ::Time.local(year, month, day).utc_offset.to_r / 86400
    else
      offset = 0
    end
    civil(year, month, day, hour, min, sec, offset)
  end

  # Converts +self+ to a floating-point number of seconds, including fractional microseconds, since the Unix epoch.
  def to_f
    seconds_since_unix_epoch.to_f + sec_fraction
  end

  # Converts +self+ to an integer number of seconds since the Unix epoch.
  def to_i
    seconds_since_unix_epoch.to_i
  end

  # Returns the fraction of a second as microseconds
  def usec
    (sec_fraction * 1_000_000).to_i
  end

  # Returns the fraction of a second as nanoseconds
  def nsec
    (sec_fraction * 1_000_000_000).to_i
  end

  private

  def offset_in_seconds
    (offset * 86400).to_i
  end

  def seconds_since_unix_epoch
    (jd - 2440588) * 86400 - offset_in_seconds + seconds_since_midnight
  end
end
require 'date'
require 'active_support/core_ext/date_and_time/zones'

class DateTime
  include DateAndTime::Zones
end
require 'securerandom'

module Digest
  module UUID
    DNS_NAMESPACE  = "k\xA7\xB8\x10\x9D\xAD\x11\xD1\x80\xB4\x00\xC0O\xD40\xC8" #:nodoc:
    URL_NAMESPACE  = "k\xA7\xB8\x11\x9D\xAD\x11\xD1\x80\xB4\x00\xC0O\xD40\xC8" #:nodoc:
    OID_NAMESPACE  = "k\xA7\xB8\x12\x9D\xAD\x11\xD1\x80\xB4\x00\xC0O\xD40\xC8" #:nodoc:
    X500_NAMESPACE = "k\xA7\xB8\x14\x9D\xAD\x11\xD1\x80\xB4\x00\xC0O\xD40\xC8" #:nodoc:

    # Generates a v5 non-random UUID (Universally Unique IDentifier).
    #
    # Using Digest::MD5 generates version 3 UUIDs; Digest::SHA1 generates version 5 UUIDs.
    # uuid_from_hash always generates the same UUID for a given name and namespace combination.
    #
    # See RFC 4122 for details of UUID at: http://www.ietf.org/rfc/rfc4122.txt
    def self.uuid_from_hash(hash_class, uuid_namespace, name)
      if hash_class == Digest::MD5
        version = 3
      elsif hash_class == Digest::SHA1
        version = 5
      else
        raise ArgumentError, "Expected Digest::SHA1 or Digest::MD5, got #{hash_class.name}."
      end

      hash = hash_class.new
      hash.update(uuid_namespace)
      hash.update(name)

      ary = hash.digest.unpack('NnnnnN')
      ary[2] = (ary[2] & 0x0FFF) | (version << 12)
      ary[3] = (ary[3] & 0x3FFF) | 0x8000

      "%08x-%04x-%04x-%04x-%04x%08x" % ary
    end

    # Convenience method for uuid_from_hash using Digest::MD5.
    def self.uuid_v3(uuid_namespace, name)
      self.uuid_from_hash(Digest::MD5, uuid_namespace, name)
    end

    # Convenience method for uuid_from_hash using Digest::SHA1.
    def self.uuid_v5(uuid_namespace, name)
      self.uuid_from_hash(Digest::SHA1, uuid_namespace, name)
    end

    # Convenience method for SecureRandom.uuid.
    def self.uuid_v4
      SecureRandom.uuid
    end
  end
end
module Enumerable
  # Calculates a sum from the elements.
  #
  #  payments.sum { |p| p.price * p.tax_rate }
  #  payments.sum(&:price)
  #
  # The latter is a shortcut for:
  #
  #  payments.inject(0) { |sum, p| sum + p.price }
  #
  # It can also calculate the sum without the use of a block.
  #
  #  [5, 15, 10].sum # => 30
  #  ['foo', 'bar'].sum # => "foobar"
  #  [[1, 2], [3, 1, 5]].sum => [1, 2, 3, 1, 5]
  #
  # The default sum of an empty list is zero. You can override this default:
  #
  #  [].sum(Payment.new(0)) { |i| i.amount } # => Payment.new(0)
  def sum(identity = 0, &block)
    if block_given?
      map(&block).sum(identity)
    else
      inject { |sum, element| sum + element } || identity
    end
  end

  # Convert an enumerable to a hash.
  #
  #   people.index_by(&:login)
  #     => { "nextangle" => <Person ...>, "chade-" => <Person ...>, ...}
  #   people.index_by { |person| "#{person.first_name} #{person.last_name}" }
  #     => { "Chade- Fowlersburg-e" => <Person ...>, "David Heinemeier Hansson" => <Person ...>, ...}
  def index_by
    if block_given?
      Hash[map { |elem| [yield(elem), elem] }]
    else
      to_enum(:index_by) { size if respond_to?(:size) }
    end
  end

  # Returns +true+ if the enumerable has more than 1 element. Functionally
  # equivalent to <tt>enum.to_a.size > 1</tt>. Can be called with a block too,
  # much like any?, so <tt>people.many? { |p| p.age > 26 }</tt> returns +true+
  # if more than one person is over 26.
  def many?
    cnt = 0
    if block_given?
      any? do |element|
        cnt += 1 if yield element
        cnt > 1
      end
    else
      any? { (cnt += 1) > 1 }
    end
  end

  # The negative of the <tt>Enumerable#include?</tt>. Returns +true+ if the
  # collection does not include the object.
  def exclude?(object)
    !include?(object)
  end

  # Returns a copy of the enumerable without the specified elements.
  #
  #   ["David", "Rafael", "Aaron", "Todd"].without "Aaron", "Todd"
  #     => ["David", "Rafael"]
  #
  #   {foo: 1, bar: 2, baz: 3}.without :bar
  #     => {foo: 1, baz: 3}
  def without(*elements)
    reject { |element| elements.include?(element) }
  end
end

class Range #:nodoc:
  # Optimize range sum to use arithmetic progression if a block is not given and
  # we have a range of numeric values.
  def sum(identity = 0)
    if block_given? || !(first.is_a?(Integer) && last.is_a?(Integer))
      super
    else
      actual_last = exclude_end? ? (last - 1) : last
      if actual_last >= first
        (actual_last - first + 1) * (actual_last + first) / 2
      else
        identity
      end
    end
  end
end
require 'active_support/core_ext/file/atomic'
require 'fileutils'

class File
  # Write to a file atomically. Useful for situations where you don't
  # want other processes or threads to see half-written files.
  #
  #   File.atomic_write('important.file') do |file|
  #     file.write('hello')
  #   end
  #
  # If your temp directory is not on the same filesystem as the file you're
  # trying to write, you can provide a different temporary directory.
  #
  #   File.atomic_write('/data/something.important', '/data/tmp') do |file|
  #     file.write('hello')
  #   end
  def self.atomic_write(file_name, temp_dir = Dir.tmpdir)
    require 'tempfile' unless defined?(Tempfile)
    require 'fileutils' unless defined?(FileUtils)

    temp_file = Tempfile.new(basename(file_name), temp_dir)
    temp_file.binmode
    return_val = yield temp_file
    temp_file.close

    if File.exist?(file_name)
      # Get original file permissions
      old_stat = stat(file_name)
    else
      # If not possible, probe which are the default permissions in the
      # destination directory.
      old_stat = probe_stat_in(dirname(file_name))
    end

    # Overwrite original file with temp file
    FileUtils.mv(temp_file.path, file_name)

    # Set correct permissions on new file
    begin
      chown(old_stat.uid, old_stat.gid, file_name)
      # This operation will affect filesystem ACL's
      chmod(old_stat.mode, file_name)

      # Make sure we return the result of the yielded block
      return_val
    rescue Errno::EPERM, Errno::EACCES
      # Changing file ownership failed, moving on.
    end
  end

  # Private utility method.
  def self.probe_stat_in(dir) #:nodoc:
    basename = [
      '.permissions_check',
      Thread.current.object_id,
      Process.pid,
      rand(1000000)
    ].join('.')

    file_name = join(dir, basename)
    FileUtils.touch(file_name)
    stat(file_name)
  ensure
    FileUtils.rm_f(file_name) if file_name
  end
end
require 'active_support/core_ext/hash/compact'
require 'active_support/core_ext/hash/conversions'
require 'active_support/core_ext/hash/deep_merge'
require 'active_support/core_ext/hash/except'
require 'active_support/core_ext/hash/indifferent_access'
require 'active_support/core_ext/hash/keys'
require 'active_support/core_ext/hash/reverse_merge'
require 'active_support/core_ext/hash/slice'
require 'active_support/core_ext/hash/transform_values'
class Hash
  # Returns a hash with non +nil+ values.
  #
  #   hash = { a: true, b: false, c: nil}
  #   hash.compact # => { a: true, b: false}
  #   hash # => { a: true, b: false, c: nil}
  #   { c: nil }.compact # => {}
  def compact
    self.select { |_, value| !value.nil? }
  end

  # Replaces current hash with non +nil+ values.
  #
  #   hash = { a: true, b: false, c: nil}
  #   hash.compact! # => { a: true, b: false}
  #   hash # => { a: true, b: false}
  def compact!
    self.reject! { |_, value| value.nil? }
  end
end
require 'active_support/xml_mini'
require 'active_support/time'
require 'active_support/core_ext/object/blank'
require 'active_support/core_ext/object/to_param'
require 'active_support/core_ext/object/to_query'
require 'active_support/core_ext/array/wrap'
require 'active_support/core_ext/hash/reverse_merge'
require 'active_support/core_ext/string/inflections'

class Hash
  # Returns a string containing an XML representation of its receiver:
  #
  #   { foo: 1, bar: 2 }.to_xml
  #   # =>
  #   # <?xml version="1.0" encoding="UTF-8"?>
  #   # <hash>
  #   #   <foo type="integer">1</foo>
  #   #   <bar type="integer">2</bar>
  #   # </hash>
  #
  # To do so, the method loops over the pairs and builds nodes that depend on
  # the _values_. Given a pair +key+, +value+:
  #
  # * If +value+ is a hash there's a recursive call with +key+ as <tt>:root</tt>.
  #
  # * If +value+ is an array there's a recursive call with +key+ as <tt>:root</tt>,
  #   and +key+ singularized as <tt>:children</tt>.
  #
  # * If +value+ is a callable object it must expect one or two arguments. Depending
  #   on the arity, the callable is invoked with the +options+ hash as first argument
  #   with +key+ as <tt>:root</tt>, and +key+ singularized as second argument. The
  #   callable can add nodes by using <tt>options[:builder]</tt>.
  #
  #     'foo'.to_xml(lambda { |options, key| options[:builder].b(key) })
  #     # => "<b>foo</b>"
  #
  # * If +value+ responds to +to_xml+ the method is invoked with +key+ as <tt>:root</tt>.
  #
  #     class Foo
  #       def to_xml(options)
  #         options[:builder].bar 'fooing!'
  #       end
  #     end
  #
  #     { foo: Foo.new }.to_xml(skip_instruct: true)
  #     # =>
  #     # <hash>
  #     #   <bar>fooing!</bar>
  #     # </hash>
  #
  # * Otherwise, a node with +key+ as tag is created with a string representation of
  #   +value+ as text node. If +value+ is +nil+ an attribute "nil" set to "true" is added.
  #   Unless the option <tt>:skip_types</tt> exists and is true, an attribute "type" is
  #   added as well according to the following mapping:
  #
  #     XML_TYPE_NAMES = {
  #       "Symbol"     => "symbol",
  #       "Fixnum"     => "integer",
  #       "Bignum"     => "integer",
  #       "BigDecimal" => "decimal",
  #       "Float"      => "float",
  #       "TrueClass"  => "boolean",
  #       "FalseClass" => "boolean",
  #       "Date"       => "date",
  #       "DateTime"   => "dateTime",
  #       "Time"       => "dateTime"
  #     }
  #
  # By default the root node is "hash", but that's configurable via the <tt>:root</tt> option.
  #
  # The default XML builder is a fresh instance of <tt>Builder::XmlMarkup</tt>. You can
  # configure your own builder with the <tt>:builder</tt> option. The method also accepts
  # options like <tt>:dasherize</tt> and friends, they are forwarded to the builder.
  def to_xml(options = {})
    require 'active_support/builder' unless defined?(Builder)

    options = options.dup
    options[:indent]  ||= 2
    options[:root]    ||= 'hash'
    options[:builder] ||= Builder::XmlMarkup.new(indent: options[:indent])

    builder = options[:builder]
    builder.instruct! unless options.delete(:skip_instruct)

    root = ActiveSupport::XmlMini.rename_key(options[:root].to_s, options)

    builder.tag!(root) do
      each { |key, value| ActiveSupport::XmlMini.to_tag(key, value, options) }
      yield builder if block_given?
    end
  end

  class << self
    # Returns a Hash containing a collection of pairs when the key is the node name and the value is
    # its content
    #
    #   xml = <<-XML
    #     <?xml version="1.0" encoding="UTF-8"?>
    #       <hash>
    #         <foo type="integer">1</foo>
    #         <bar type="integer">2</bar>
    #       </hash>
    #   XML
    #
    #   hash = Hash.from_xml(xml)
    #   # => {"hash"=>{"foo"=>1, "bar"=>2}}
    #
    # +DisallowedType+ is raised if the XML contains attributes with <tt>type="yaml"</tt> or
    # <tt>type="symbol"</tt>. Use <tt>Hash.from_trusted_xml</tt> to parse this XML.
    def from_xml(xml, disallowed_types = nil)
      ActiveSupport::XMLConverter.new(xml, disallowed_types).to_h
    end

    # Builds a Hash from XML just like <tt>Hash.from_xml</tt>, but also allows Symbol and YAML.
    def from_trusted_xml(xml)
      from_xml xml, []
    end
  end
end

module ActiveSupport
  class XMLConverter # :nodoc:
    class DisallowedType < StandardError
      def initialize(type)
        super "Disallowed type attribute: #{type.inspect}"
      end
    end

    DISALLOWED_TYPES = %w(symbol yaml)

    def initialize(xml, disallowed_types = nil)
      @xml = normalize_keys(XmlMini.parse(xml))
      @disallowed_types = disallowed_types || DISALLOWED_TYPES
    end

    def to_h
      deep_to_h(@xml)
    end

    private
      def normalize_keys(params)
        case params
          when Hash
            Hash[params.map { |k,v| [k.to_s.tr('-', '_'), normalize_keys(v)] } ]
          when Array
            params.map { |v| normalize_keys(v) }
          else
            params
        end
      end

      def deep_to_h(value)
        case value
          when Hash
            process_hash(value)
          when Array
            process_array(value)
          when String
            value
          else
            raise "can't typecast #{value.class.name} - #{value.inspect}"
        end
      end

      def process_hash(value)
        if value.include?('type') && !value['type'].is_a?(Hash) && @disallowed_types.include?(value['type'])
          raise DisallowedType, value['type']
        end

        if become_array?(value)
          _, entries = Array.wrap(value.detect { |k,v| not v.is_a?(String) })
          if entries.nil? || value['__content__'].try(:empty?)
            []
          else
            case entries
            when Array
              entries.collect { |v| deep_to_h(v) }
            when Hash
              [deep_to_h(entries)]
            else
              raise "can't typecast #{entries.inspect}"
            end
          end
        elsif become_content?(value)
          process_content(value)

        elsif become_empty_string?(value)
          ''
        elsif become_hash?(value)
          xml_value = Hash[value.map { |k,v| [k, deep_to_h(v)] }]

          # Turn { files: { file: #<StringIO> } } into { files: #<StringIO> } so it is compatible with
          # how multipart uploaded files from HTML appear
          xml_value['file'].is_a?(StringIO) ? xml_value['file'] : xml_value
        end
      end

      def become_content?(value)
        value['type'] == 'file' || (value['__content__'] && (value.keys.size == 1 || value['__content__'].present?))
      end

      def become_array?(value)
        value['type'] == 'array'
      end

      def become_empty_string?(value)
        # { "string" => true }
        # No tests fail when the second term is removed.
        value['type'] == 'string' && value['nil'] != 'true'
      end

      def become_hash?(value)
        !nothing?(value) && !garbage?(value)
      end

      def nothing?(value)
        # blank or nil parsed values are represented by nil
        value.blank? || value['nil'] == 'true'
      end

      def garbage?(value)
        # If the type is the only element which makes it then
        # this still makes the value nil, except if type is
        # an XML node(where type['value'] is a Hash)
        value['type'] && !value['type'].is_a?(::Hash) && value.size == 1
      end

      def process_content(value)
        content = value['__content__']
        if parser = ActiveSupport::XmlMini::PARSING[value['type']]
          parser.arity == 1 ? parser.call(content) : parser.call(content, value)
        else
          content
        end
      end

      def process_array(value)
        value.map! { |i| deep_to_h(i) }
        value.length > 1 ? value : value.first
      end

  end
end
class Hash
  # Returns a new hash with +self+ and +other_hash+ merged recursively.
  #
  #   h1 = { a: true, b: { c: [1, 2, 3] } }
  #   h2 = { a: false, b: { x: [3, 4, 5] } }
  #
  #   h1.deep_merge(h2) # => { a: false, b: { c: [1, 2, 3], x: [3, 4, 5] } }
  #
  # Like with Hash#merge in the standard library, a block can be provided
  # to merge values:
  #
  #   h1 = { a: 100, b: 200, c: { c1: 100 } }
  #   h2 = { b: 250, c: { c1: 200 } }
  #   h1.deep_merge(h2) { |key, this_val, other_val| this_val + other_val }
  #   # => { a: 100, b: 450, c: { c1: 300 } }
  def deep_merge(other_hash, &block)
    dup.deep_merge!(other_hash, &block)
  end

  # Same as +deep_merge+, but modifies +self+.
  def deep_merge!(other_hash, &block)
    other_hash.each_pair do |current_key, other_value|
      this_value = self[current_key]

      self[current_key] = if this_value.is_a?(Hash) && other_value.is_a?(Hash)
        this_value.deep_merge(other_value, &block)
      else
        if block_given? && key?(current_key)
          block.call(current_key, this_value, other_value)
        else
          other_value
        end
      end
    end

    self
  end
end
class Hash
  # Returns a hash that includes everything but the given keys.
  #   hash = { a: true, b: false, c: nil}
  #   hash.except(:c) # => { a: true, b: false}
  #   hash # => { a: true, b: false, c: nil}
  #
  # This is useful for limiting a set of parameters to everything but a few known toggles:
  #   @person.update(params[:person].except(:admin))
  def except(*keys)
    dup.except!(*keys)
  end

  # Replaces the hash without the given keys.
  #   hash = { a: true, b: false, c: nil}
  #   hash.except!(:c) # => { a: true, b: false}
  #   hash # => { a: true, b: false }
  def except!(*keys)
    keys.each { |key| delete(key) }
    self
  end
end
require 'active_support/hash_with_indifferent_access'

class Hash

  # Returns an <tt>ActiveSupport::HashWithIndifferentAccess</tt> out of its receiver:
  #
  #   { a: 1 }.with_indifferent_access['a'] # => 1
  def with_indifferent_access
    ActiveSupport::HashWithIndifferentAccess.new_from_hash_copying_default(self)
  end

  # Called when object is nested under an object that receives
  # #with_indifferent_access. This method will be called on the current object
  # by the enclosing object and is aliased to #with_indifferent_access by
  # default. Subclasses of Hash may overwrite this method to return +self+ if
  # converting to an <tt>ActiveSupport::HashWithIndifferentAccess</tt> would not be
  # desirable.
  #
  #   b = { b: 1 }
  #   { a: b }.with_indifferent_access['a'] # calls b.nested_under_indifferent_access
  #   # => {"b"=>1}
  alias nested_under_indifferent_access with_indifferent_access
end
class Hash
  # Returns a new hash with all keys converted using the block operation.
  #
  #  hash = { name: 'Rob', age: '28' }
  #
  #  hash.transform_keys{ |key| key.to_s.upcase }
  #  # => {"NAME"=>"Rob", "AGE"=>"28"}
  def transform_keys
    return enum_for(:transform_keys) unless block_given?
    result = self.class.new
    each_key do |key|
      result[yield(key)] = self[key]
    end
    result
  end

  # Destructively converts all keys using the block operations.
  # Same as transform_keys but modifies +self+.
  def transform_keys!
    return enum_for(:transform_keys!) unless block_given?
    keys.each do |key|
      self[yield(key)] = delete(key)
    end
    self
  end

  # Returns a new hash with all keys converted to strings.
  #
  #   hash = { name: 'Rob', age: '28' }
  #
  #   hash.stringify_keys
  #   # => {"name"=>"Rob", "age"=>"28"}
  def stringify_keys
    transform_keys(&:to_s)
  end

  # Destructively converts all keys to strings. Same as
  # +stringify_keys+, but modifies +self+.
  def stringify_keys!
    transform_keys!(&:to_s)
  end

  # Returns a new hash with all keys converted to symbols, as long as
  # they respond to +to_sym+.
  #
  #   hash = { 'name' => 'Rob', 'age' => '28' }
  #
  #   hash.symbolize_keys
  #   # => {:name=>"Rob", :age=>"28"}
  def symbolize_keys
    transform_keys{ |key| key.to_sym rescue key }
  end
  alias_method :to_options,  :symbolize_keys

  # Destructively converts all keys to symbols, as long as they respond
  # to +to_sym+. Same as +symbolize_keys+, but modifies +self+.
  def symbolize_keys!
    transform_keys!{ |key| key.to_sym rescue key }
  end
  alias_method :to_options!, :symbolize_keys!

  # Validates all keys in a hash match <tt>*valid_keys</tt>, raising
  # ArgumentError on a mismatch.
  #
  # Note that keys are treated differently than HashWithIndifferentAccess,
  # meaning that string and symbol keys will not match.
  #
  #   { name: 'Rob', years: '28' }.assert_valid_keys(:name, :age) # => raises "ArgumentError: Unknown key: :years. Valid keys are: :name, :age"
  #   { name: 'Rob', age: '28' }.assert_valid_keys('name', 'age') # => raises "ArgumentError: Unknown key: :name. Valid keys are: 'name', 'age'"
  #   { name: 'Rob', age: '28' }.assert_valid_keys(:name, :age)   # => passes, raises nothing
  def assert_valid_keys(*valid_keys)
    valid_keys.flatten!
    each_key do |k|
      unless valid_keys.include?(k)
        raise ArgumentError.new("Unknown key: #{k.inspect}. Valid keys are: #{valid_keys.map(&:inspect).join(', ')}")
      end
    end
  end

  # Returns a new hash with all keys converted by the block operation.
  # This includes the keys from the root hash and from all
  # nested hashes and arrays.
  #
  #  hash = { person: { name: 'Rob', age: '28' } }
  #
  #  hash.deep_transform_keys{ |key| key.to_s.upcase }
  #  # => {"PERSON"=>{"NAME"=>"Rob", "AGE"=>"28"}}
  def deep_transform_keys(&block)
    _deep_transform_keys_in_object(self, &block)
  end

  # Destructively converts all keys by using the block operation.
  # This includes the keys from the root hash and from all
  # nested hashes and arrays.
  def deep_transform_keys!(&block)
    _deep_transform_keys_in_object!(self, &block)
  end

  # Returns a new hash with all keys converted to strings.
  # This includes the keys from the root hash and from all
  # nested hashes and arrays.
  #
  #   hash = { person: { name: 'Rob', age: '28' } }
  #
  #   hash.deep_stringify_keys
  #   # => {"person"=>{"name"=>"Rob", "age"=>"28"}}
  def deep_stringify_keys
    deep_transform_keys(&:to_s)
  end

  # Destructively converts all keys to strings.
  # This includes the keys from the root hash and from all
  # nested hashes and arrays.
  def deep_stringify_keys!
    deep_transform_keys!(&:to_s)
  end

  # Returns a new hash with all keys converted to symbols, as long as
  # they respond to +to_sym+. This includes the keys from the root hash
  # and from all nested hashes and arrays.
  #
  #   hash = { 'person' => { 'name' => 'Rob', 'age' => '28' } }
  #
  #   hash.deep_symbolize_keys
  #   # => {:person=>{:name=>"Rob", :age=>"28"}}
  def deep_symbolize_keys
    deep_transform_keys{ |key| key.to_sym rescue key }
  end

  # Destructively converts all keys to symbols, as long as they respond
  # to +to_sym+. This includes the keys from the root hash and from all
  # nested hashes and arrays.
  def deep_symbolize_keys!
    deep_transform_keys!{ |key| key.to_sym rescue key }
  end

  private
    # support methods for deep transforming nested hashes and arrays
    def _deep_transform_keys_in_object(object, &block)
      case object
      when Hash
        object.each_with_object({}) do |(key, value), result|
          result[yield(key)] = _deep_transform_keys_in_object(value, &block)
        end
      when Array
        object.map {|e| _deep_transform_keys_in_object(e, &block) }
      else
        object
      end
    end

    def _deep_transform_keys_in_object!(object, &block)
      case object
      when Hash
        object.keys.each do |key|
          value = object.delete(key)
          object[yield(key)] = _deep_transform_keys_in_object!(value, &block)
        end
        object
      when Array
        object.map! {|e| _deep_transform_keys_in_object!(e, &block)}
      else
        object
      end
    end
end
class Hash
  # Merges the caller into +other_hash+. For example,
  #
  #   options = options.reverse_merge(size: 25, velocity: 10)
  #
  # is equivalent to
  #
  #   options = { size: 25, velocity: 10 }.merge(options)
  #
  # This is particularly useful for initializing an options hash
  # with default values.
  def reverse_merge(other_hash)
    other_hash.merge(self)
  end

  # Destructive +reverse_merge+.
  def reverse_merge!(other_hash)
    # right wins if there is no left
    merge!( other_hash ){|key,left,right| left }
  end
  alias_method :reverse_update, :reverse_merge!
end
class Hash
  # Slices a hash to include only the given keys. Returns a hash containing 
  # the given keys.
  # 
  #   { a: 1, b: 2, c: 3, d: 4 }.slice(:a, :b)
  #   # => {:a=>1, :b=>2}
  # 
  # This is useful for limiting an options hash to valid keys before 
  # passing to a method:
  #
  #   def search(criteria = {})
  #     criteria.assert_valid_keys(:mass, :velocity, :time)
  #   end
  #
  #   search(options.slice(:mass, :velocity, :time))
  #
  # If you have an array of keys you want to limit to, you should splat them:
  #
  #   valid_keys = [:mass, :velocity, :time]
  #   search(options.slice(*valid_keys))
  def slice(*keys)
    keys.map! { |key| convert_key(key) } if respond_to?(:convert_key, true)
    keys.each_with_object(self.class.new) { |k, hash| hash[k] = self[k] if has_key?(k) }
  end

  # Replaces the hash with only the given keys.
  # Returns a hash containing the removed key/value pairs.
  #
  #   { a: 1, b: 2, c: 3, d: 4 }.slice!(:a, :b)
  #   # => {:c=>3, :d=>4}
  def slice!(*keys)
    keys.map! { |key| convert_key(key) } if respond_to?(:convert_key, true)
    omit = slice(*self.keys - keys)
    hash = slice(*keys)
    hash.default      = default
    hash.default_proc = default_proc if default_proc
    replace(hash)
    omit
  end

  # Removes and returns the key/value pairs matching the given keys.
  #
  #   { a: 1, b: 2, c: 3, d: 4 }.extract!(:a, :b) # => {:a=>1, :b=>2}
  #   { a: 1, b: 2 }.extract!(:a, :x)             # => {:a=>1}
  def extract!(*keys)
    keys.each_with_object(self.class.new) { |key, result| result[key] = delete(key) if has_key?(key) }
  end
end
class Hash
  # Returns a new hash with the results of running +block+ once for every value.
  # The keys are unchanged.
  #
  #   { a: 1, b: 2, c: 3 }.transform_values { |x| x * 2 }
  #   # => { a: 2, b: 4, c: 6 }
  def transform_values
    return enum_for(:transform_values) unless block_given?
    result = self.class.new
    each do |key, value|
      result[key] = yield(value)
    end
    result
  end

  # Destructive +transform_values+
  def transform_values!
    return enum_for(:transform_values!) unless block_given?
    each do |key, value|
      self[key] = yield(value)
    end
  end
end
require 'active_support/core_ext/integer/multiple'
require 'active_support/core_ext/integer/inflections'
require 'active_support/core_ext/integer/time'
require 'active_support/inflector'

class Integer
  # Ordinalize turns a number into an ordinal string used to denote the
  # position in an ordered sequence such as 1st, 2nd, 3rd, 4th.
  #
  #  1.ordinalize     # => "1st"
  #  2.ordinalize     # => "2nd"
  #  1002.ordinalize  # => "1002nd"
  #  1003.ordinalize  # => "1003rd"
  #  -11.ordinalize   # => "-11th"
  #  -1001.ordinalize # => "-1001st"
  def ordinalize
    ActiveSupport::Inflector.ordinalize(self)
  end

  # Ordinal returns the suffix used to denote the position
  # in an ordered sequence such as 1st, 2nd, 3rd, 4th.
  #
  #  1.ordinal     # => "st"
  #  2.ordinal     # => "nd"
  #  1002.ordinal  # => "nd"
  #  1003.ordinal  # => "rd"
  #  -11.ordinal   # => "th"
  #  -1001.ordinal # => "st"
  def ordinal
    ActiveSupport::Inflector.ordinal(self)
  end
end
class Integer
  # Check whether the integer is evenly divisible by the argument.
  #
  #   0.multiple_of?(0)  # => true
  #   6.multiple_of?(5)  # => false
  #   10.multiple_of?(2) # => true
  def multiple_of?(number)
    number != 0 ? self % number == 0 : zero?
  end
end
require 'active_support/duration'
require 'active_support/core_ext/numeric/time'

class Integer
  # Enables the use of time calculations and declarations, like <tt>45.minutes +
  # 2.hours + 4.years</tt>.
  #
  # These methods use Time#advance for precise date calculations when using
  # <tt>from_now</tt>, +ago+, etc. as well as adding or subtracting their
  # results from a Time object.
  #
  #   # equivalent to Time.now.advance(months: 1)
  #   1.month.from_now
  #
  #   # equivalent to Time.now.advance(years: 2)
  #   2.years.from_now
  #
  #   # equivalent to Time.now.advance(months: 4, years: 5)
  #   (4.months + 5.years).from_now
  def months
    ActiveSupport::Duration.new(self * 30.days, [[:months, self]])
  end
  alias :month :months

  def years
    ActiveSupport::Duration.new(self * 365.25.days, [[:years, self]])
  end
  alias :year :years
end
require 'active_support/core_ext/kernel/agnostics'
require 'active_support/core_ext/kernel/concern'
require 'active_support/core_ext/kernel/reporting'
require 'active_support/core_ext/kernel/singleton_class'
class Object
  # Makes backticks behave (somewhat more) similarly on all platforms.
  # On win32 `nonexistent_command` raises Errno::ENOENT; on Unix, the
  # spawned shell prints a message to stderr and sets $?. We emulate
  # Unix on the former but not the latter.
  def `(command) #:nodoc:
    super
  rescue Errno::ENOENT => e
    STDERR.puts "#$0: #{e}"
  end
end
require 'active_support/core_ext/module/concerning'

module Kernel
  # A shortcut to define a toplevel concern, not within a module.
  #
  # See Module::Concerning for more.
  def concern(topic, &module_definition)
    Object.concern topic, &module_definition
  end
end
require 'active_support/deprecation'

ActiveSupport::Deprecation.warn("This file is deprecated and will be removed in Rails 5.1 with no replacement.")
require 'tempfile'

module Kernel
  # Sets $VERBOSE to nil for the duration of the block and back to its original
  # value afterwards.
  #
  #   silence_warnings do
  #     value = noisy_call # no warning voiced
  #   end
  #
  #   noisy_call # warning voiced
  def silence_warnings
    with_warnings(nil) { yield }
  end

  # Sets $VERBOSE to +true+ for the duration of the block and back to its
  # original value afterwards.
  def enable_warnings
    with_warnings(true) { yield }
  end

  # Sets $VERBOSE for the duration of the block and back to its original
  # value afterwards.
  def with_warnings(flag)
    old_verbose, $VERBOSE = $VERBOSE, flag
    yield
  ensure
    $VERBOSE = old_verbose
  end

  # Blocks and ignores any exception passed as argument if raised within the block.
  #
  #   suppress(ZeroDivisionError) do
  #     1/0
  #     puts 'This code is NOT reached'
  #   end
  #
  #   puts 'This code gets executed and nothing related to ZeroDivisionError was seen'
  def suppress(*exception_classes)
    yield
  rescue *exception_classes
  end
end
module Kernel
  # class_eval on an object acts like singleton_class.class_eval.
  def class_eval(*args, &block)
    singleton_class.class_eval(*args, &block)
  end
end
require 'active_support/deprecation/proxy_wrappers'

class LoadError
  REGEXPS = [
    /^no such file to load -- (.+)$/i,
    /^Missing \w+ (?:file\s*)?([^\s]+.rb)$/i,
    /^Missing API definition file in (.+)$/i,
    /^cannot load such file -- (.+)$/i,
  ]

  unless method_defined?(:path)
    # Returns the path which was unable to be loaded.
    def path
      @path ||= begin
        REGEXPS.find do |regex|
          message =~ regex
        end
        $1
      end
    end
  end

  # Returns true if the given path name (except perhaps for the ".rb"
  # extension) is the missing file which caused the exception to be raised.
  def is_missing?(location)
    location.sub(/\.rb$/, '') == path.sub(/\.rb$/, '')
  end
end

MissingSourceFile = ActiveSupport::Deprecation::DeprecatedConstantProxy.new('MissingSourceFile', 'LoadError')
module ActiveSupport
  module MarshalWithAutoloading # :nodoc:
    def load(source)
      super(source)
    rescue ArgumentError, NameError => exc
      if exc.message.match(%r|undefined class/module (.+)|)
        # try loading the class/module
        $1.constantize
        # if it is a IO we need to go back to read the object
        source.rewind if source.respond_to?(:rewind)
        retry
      else
        raise exc
      end
    end
  end
end

Marshal.singleton_class.prepend(ActiveSupport::MarshalWithAutoloading)
require 'active_support/core_ext/module/aliasing'
require 'active_support/core_ext/module/introspection'
require 'active_support/core_ext/module/anonymous'
require 'active_support/core_ext/module/reachable'
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/core_ext/module/attr_internal'
require 'active_support/core_ext/module/concerning'
require 'active_support/core_ext/module/delegation'
require 'active_support/core_ext/module/deprecation'
require 'active_support/core_ext/module/remove_method'
require 'active_support/core_ext/module/qualified_const'
class Module
  # NOTE: This method is deprecated. Please use <tt>Module#prepend</tt> that
  # comes with Ruby 2.0 or newer instead.
  #
  # Encapsulates the common pattern of:
  #
  #   alias_method :foo_without_feature, :foo
  #   alias_method :foo, :foo_with_feature
  #
  # With this, you simply do:
  #
  #   alias_method_chain :foo, :feature
  #
  # And both aliases are set up for you.
  #
  # Query and bang methods (foo?, foo!) keep the same punctuation:
  #
  #   alias_method_chain :foo?, :feature
  #
  # is equivalent to
  #
  #   alias_method :foo_without_feature?, :foo?
  #   alias_method :foo?, :foo_with_feature?
  #
  # so you can safely chain foo, foo?, foo! and/or foo= with the same feature.
  def alias_method_chain(target, feature)
    ActiveSupport::Deprecation.warn("alias_method_chain is deprecated. Please, use Module#prepend instead. From module, you can access the original method using super.")

    # Strip out punctuation on predicates, bang or writer methods since
    # e.g. target?_without_feature is not a valid method name.
    aliased_target, punctuation = target.to_s.sub(/([?!=])$/, ''), $1
    yield(aliased_target, punctuation) if block_given?

    with_method = "#{aliased_target}_with_#{feature}#{punctuation}"
    without_method = "#{aliased_target}_without_#{feature}#{punctuation}"

    alias_method without_method, target
    alias_method target, with_method

    case
    when public_method_defined?(without_method)
      public target
    when protected_method_defined?(without_method)
      protected target
    when private_method_defined?(without_method)
      private target
    end
  end

  # Allows you to make aliases for attributes, which includes
  # getter, setter, and a predicate.
  #
  #   class Content < ActiveRecord::Base
  #     # has a title attribute
  #   end
  #
  #   class Email < Content
  #     alias_attribute :subject, :title
  #   end
  #
  #   e = Email.find(1)
  #   e.title    # => "Superstars"
  #   e.subject  # => "Superstars"
  #   e.subject? # => true
  #   e.subject = "Megastars"
  #   e.title    # => "Megastars"
  def alias_attribute(new_name, old_name)
    module_eval <<-STR, __FILE__, __LINE__ + 1
      def #{new_name}; self.#{old_name}; end          # def subject; self.title; end
      def #{new_name}?; self.#{old_name}?; end        # def subject?; self.title?; end
      def #{new_name}=(v); self.#{old_name} = v; end  # def subject=(v); self.title = v; end
    STR
  end
end
class Module
  # A module may or may not have a name.
  #
  #   module M; end
  #   M.name # => "M"
  #
  #   m = Module.new
  #   m.name # => nil
  #
  # +anonymous?+ method returns true if module does not have a name:
  #
  #   Module.new.anonymous? # => true
  #
  #   module M; end
  #   M.anonymous?          # => false
  #
  # A module gets a name when it is first assigned to a constant. Either
  # via the +module+ or +class+ keyword or by an explicit assignment:
  #
  #   m = Module.new # creates an anonymous module
  #   M = m          # => m gets a name here as a side-effect
  #   m.name         # => "M"
  def anonymous?
    name.nil?
  end
end
class Module
  # Declares an attribute reader backed by an internally-named instance variable.
  def attr_internal_reader(*attrs)
    attrs.each {|attr_name| attr_internal_define(attr_name, :reader)}
  end

  # Declares an attribute writer backed by an internally-named instance variable.
  def attr_internal_writer(*attrs)
    attrs.each {|attr_name| attr_internal_define(attr_name, :writer)}
  end

  # Declares an attribute reader and writer backed by an internally-named instance
  # variable.
  def attr_internal_accessor(*attrs)
    attr_internal_reader(*attrs)
    attr_internal_writer(*attrs)
  end
  alias_method :attr_internal, :attr_internal_accessor

  class << self; attr_accessor :attr_internal_naming_format end
  self.attr_internal_naming_format = '@_%s'

  private
    def attr_internal_ivar_name(attr)
      Module.attr_internal_naming_format % attr
    end

    def attr_internal_define(attr_name, type)
      internal_name = attr_internal_ivar_name(attr_name).sub(/\A@/, '')
      # use native attr_* methods as they are faster on some Ruby implementations
      send("attr_#{type}", internal_name)
      attr_name, internal_name = "#{attr_name}=", "#{internal_name}=" if type == :writer
      alias_method attr_name, internal_name
      remove_method internal_name
    end
end
require 'active_support/core_ext/array/extract_options'

# Extends the module object with class/module and instance accessors for
# class/module attributes, just like the native attr* accessors for instance
# attributes.
class Module
  # Defines a class attribute and creates a class and instance reader methods.
  # The underlying the class variable is set to +nil+, if it is not previously
  # defined.
  #
  #   module HairColors
  #     mattr_reader :hair_colors
  #   end
  #
  #   HairColors.hair_colors # => nil
  #   HairColors.class_variable_set("@@hair_colors", [:brown, :black])
  #   HairColors.hair_colors # => [:brown, :black]
  #
  # The attribute name must be a valid method name in Ruby.
  #
  #   module Foo
  #     mattr_reader :"1_Badname "
  #   end
  #   # => NameError: invalid attribute name
  #
  # If you want to opt out the creation on the instance reader method, pass
  # <tt>instance_reader: false</tt> or <tt>instance_accessor: false</tt>.
  #
  #   module HairColors
  #     mattr_writer :hair_colors, instance_reader: false
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.new.hair_colors # => NoMethodError
  #
  #
  # Also, you can pass a block to set up the attribute with a default value.
  #
  #   module HairColors
  #     cattr_reader :hair_colors do
  #       [:brown, :black, :blonde, :red]
  #     end
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.hair_colors # => [:brown, :black, :blonde, :red]
  def mattr_reader(*syms)
    options = syms.extract_options!
    syms.each do |sym|
      raise NameError.new("invalid attribute name: #{sym}") unless sym =~ /^[_A-Za-z]\w*$/
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        @@#{sym} = nil unless defined? @@#{sym}

        def self.#{sym}
          @@#{sym}
        end
      EOS

      unless options[:instance_reader] == false || options[:instance_accessor] == false
        class_eval(<<-EOS, __FILE__, __LINE__ + 1)
          def #{sym}
            @@#{sym}
          end
        EOS
      end
      class_variable_set("@@#{sym}", yield) if block_given?
    end
  end
  alias :cattr_reader :mattr_reader

  # Defines a class attribute and creates a class and instance writer methods to
  # allow assignment to the attribute.
  #
  #   module HairColors
  #     mattr_writer :hair_colors
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   HairColors.hair_colors = [:brown, :black]
  #   Person.class_variable_get("@@hair_colors") # => [:brown, :black]
  #   Person.new.hair_colors = [:blonde, :red]
  #   HairColors.class_variable_get("@@hair_colors") # => [:blonde, :red]
  #
  # If you want to opt out the instance writer method, pass
  # <tt>instance_writer: false</tt> or <tt>instance_accessor: false</tt>.
  #
  #   module HairColors
  #     mattr_writer :hair_colors, instance_writer: false
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.new.hair_colors = [:blonde, :red] # => NoMethodError
  #
  # Also, you can pass a block to set up the attribute with a default value.
  #
  #   class HairColors
  #     mattr_writer :hair_colors do
  #       [:brown, :black, :blonde, :red]
  #     end
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.class_variable_get("@@hair_colors") # => [:brown, :black, :blonde, :red]
  def mattr_writer(*syms)
    options = syms.extract_options!
    syms.each do |sym|
      raise NameError.new("invalid attribute name: #{sym}") unless sym =~ /^[_A-Za-z]\w*$/
      class_eval(<<-EOS, __FILE__, __LINE__ + 1)
        @@#{sym} = nil unless defined? @@#{sym}

        def self.#{sym}=(obj)
          @@#{sym} = obj
        end
      EOS

      unless options[:instance_writer] == false || options[:instance_accessor] == false
        class_eval(<<-EOS, __FILE__, __LINE__ + 1)
          def #{sym}=(obj)
            @@#{sym} = obj
          end
        EOS
      end
      send("#{sym}=", yield) if block_given?
    end
  end
  alias :cattr_writer :mattr_writer

  # Defines both class and instance accessors for class attributes.
  #
  #   module HairColors
  #     mattr_accessor :hair_colors
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.hair_colors = [:brown, :black, :blonde, :red]
  #   Person.hair_colors     # => [:brown, :black, :blonde, :red]
  #   Person.new.hair_colors # => [:brown, :black, :blonde, :red]
  #
  # If a subclass changes the value then that would also change the value for
  # parent class. Similarly if parent class changes the value then that would
  # change the value of subclasses too.
  #
  #   class Male < Person
  #   end
  #
  #   Male.hair_colors << :blue
  #   Person.hair_colors # => [:brown, :black, :blonde, :red, :blue]
  #
  # To opt out of the instance writer method, pass <tt>instance_writer: false</tt>.
  # To opt out of the instance reader method, pass <tt>instance_reader: false</tt>.
  #
  #   module HairColors
  #     mattr_accessor :hair_colors, instance_writer: false, instance_reader: false
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.new.hair_colors = [:brown]  # => NoMethodError
  #   Person.new.hair_colors             # => NoMethodError
  #
  # Or pass <tt>instance_accessor: false</tt>, to opt out both instance methods.
  #
  #   module HairColors
  #     mattr_accessor :hair_colors, instance_accessor: false
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.new.hair_colors = [:brown]  # => NoMethodError
  #   Person.new.hair_colors             # => NoMethodError
  #
  # Also you can pass a block to set up the attribute with a default value.
  #
  #   module HairColors
  #     mattr_accessor :hair_colors do
  #       [:brown, :black, :blonde, :red]
  #     end
  #   end
  #
  #   class Person
  #     include HairColors
  #   end
  #
  #   Person.class_variable_get("@@hair_colors") # => [:brown, :black, :blonde, :red]
  def mattr_accessor(*syms, &blk)
    mattr_reader(*syms, &blk)
    mattr_writer(*syms, &blk)
  end
  alias :cattr_accessor :mattr_accessor
end
require 'active_support/concern'

class Module
  # = Bite-sized separation of concerns
  #
  # We often find ourselves with a medium-sized chunk of behavior that we'd
  # like to extract, but only mix in to a single class.
  #
  # Extracting a plain old Ruby object to encapsulate it and collaborate or
  # delegate to the original object is often a good choice, but when there's
  # no additional state to encapsulate or we're making DSL-style declarations
  # about the parent class, introducing new collaborators can obfuscate rather
  # than simplify.
  #
  # The typical route is to just dump everything in a monolithic class, perhaps
  # with a comment, as a least-bad alternative. Using modules in separate files
  # means tedious sifting to get a big-picture view.
  #
  # = Dissatisfying ways to separate small concerns
  #
  # == Using comments:
  #
  #   class Todo
  #     # Other todo implementation
  #     # ...
  #
  #     ## Event tracking
  #     has_many :events
  #
  #     before_create :track_creation
  #     after_destroy :track_deletion
  #
  #     private
  #       def track_creation
  #         # ...
  #       end
  #   end
  #
  # == With an inline module:
  #
  # Noisy syntax.
  #
  #   class Todo
  #     # Other todo implementation
  #     # ...
  #
  #     module EventTracking
  #       extend ActiveSupport::Concern
  #
  #       included do
  #         has_many :events
  #         before_create :track_creation
  #         after_destroy :track_deletion
  #       end
  #
  #       private
  #         def track_creation
  #           # ...
  #         end
  #     end
  #     include EventTracking
  #   end
  #
  # == Mix-in noise exiled to its own file:
  #
  # Once our chunk of behavior starts pushing the scroll-to-understand-it
  # boundary, we give in and move it to a separate file. At this size, the
  # increased overhead can be a reasonable tradeoff even if it reduces our
  # at-a-glance perception of how things work.
  #
  #   class Todo
  #     # Other todo implementation
  #     # ...
  #
  #     include TodoEventTracking
  #   end
  #
  # = Introducing Module#concerning
  #
  # By quieting the mix-in noise, we arrive at a natural, low-ceremony way to
  # separate bite-sized concerns.
  #
  #   class Todo
  #     # Other todo implementation
  #     # ...
  #
  #     concerning :EventTracking do
  #       included do
  #         has_many :events
  #         before_create :track_creation
  #         after_destroy :track_deletion
  #       end
  #
  #       private
  #         def track_creation
  #           # ...
  #         end
  #     end
  #   end
  #
  #   Todo.ancestors
  #   # => Todo, Todo::EventTracking, Object
  #
  # This small step has some wonderful ripple effects. We can
  # * grok the behavior of our class in one glance,
  # * clean up monolithic junk-drawer classes by separating their concerns, and
  # * stop leaning on protected/private for crude "this is internal stuff" modularity.
  module Concerning
    # Define a new concern and mix it in.
    def concerning(topic, &block)
      include concern(topic, &block)
    end

    # A low-cruft shortcut to define a concern.
    #
    #   concern :EventTracking do
    #     ...
    #   end
    #
    # is equivalent to
    #
    #   module EventTracking
    #     extend ActiveSupport::Concern
    #
    #     ...
    #   end
    def concern(topic, &module_definition)
      const_set topic, Module.new {
        extend ::ActiveSupport::Concern
        module_eval(&module_definition)
      }
    end
  end
  include Concerning
end
require 'set'

class Module
  # Error generated by +delegate+ when a method is called on +nil+ and +allow_nil+
  # option is not used.
  class DelegationError < NoMethodError; end

  RUBY_RESERVED_WORDS = Set.new(
    %w(alias and BEGIN begin break case class def defined? do else elsif END
       end ensure false for if in module next nil not or redo rescue retry
       return self super then true undef unless until when while yield)
  ).freeze

  # Provides a +delegate+ class method to easily expose contained objects'
  # public methods as your own.
  #
  # ==== Options
  # * <tt>:to</tt> - Specifies the target object
  # * <tt>:prefix</tt> - Prefixes the new method with the target name or a custom prefix
  # * <tt>:allow_nil</tt> - if set to true, prevents a +NoMethodError+ from being raised
  #
  # The macro receives one or more method names (specified as symbols or
  # strings) and the name of the target object via the <tt>:to</tt> option
  # (also a symbol or string).
  #
  # Delegation is particularly useful with Active Record associations:
  #
  #   class Greeter < ActiveRecord::Base
  #     def hello
  #       'hello'
  #     end
  #
  #     def goodbye
  #       'goodbye'
  #     end
  #   end
  #
  #   class Foo < ActiveRecord::Base
  #     belongs_to :greeter
  #     delegate :hello, to: :greeter
  #   end
  #
  #   Foo.new.hello   # => "hello"
  #   Foo.new.goodbye # => NoMethodError: undefined method `goodbye' for #<Foo:0x1af30c>
  #
  # Multiple delegates to the same target are allowed:
  #
  #   class Foo < ActiveRecord::Base
  #     belongs_to :greeter
  #     delegate :hello, :goodbye, to: :greeter
  #   end
  #
  #   Foo.new.goodbye # => "goodbye"
  #
  # Methods can be delegated to instance variables, class variables, or constants
  # by providing them as a symbols:
  #
  #   class Foo
  #     CONSTANT_ARRAY = [0,1,2,3]
  #     @@class_array  = [4,5,6,7]
  #
  #     def initialize
  #       @instance_array = [8,9,10,11]
  #     end
  #     delegate :sum, to: :CONSTANT_ARRAY
  #     delegate :min, to: :@@class_array
  #     delegate :max, to: :@instance_array
  #   end
  #
  #   Foo.new.sum # => 6
  #   Foo.new.min # => 4
  #   Foo.new.max # => 11
  #
  # It's also possible to delegate a method to the class by using +:class+:
  #
  #   class Foo
  #     def self.hello
  #       "world"
  #     end
  #
  #     delegate :hello, to: :class
  #   end
  #
  #   Foo.new.hello # => "world"
  #
  # Delegates can optionally be prefixed using the <tt>:prefix</tt> option. If the value
  # is <tt>true</tt>, the delegate methods are prefixed with the name of the object being
  # delegated to.
  #
  #   Person = Struct.new(:name, :address)
  #
  #   class Invoice < Struct.new(:client)
  #     delegate :name, :address, to: :client, prefix: true
  #   end
  #
  #   john_doe = Person.new('John Doe', 'Vimmersvej 13')
  #   invoice = Invoice.new(john_doe)
  #   invoice.client_name    # => "John Doe"
  #   invoice.client_address # => "Vimmersvej 13"
  #
  # It is also possible to supply a custom prefix.
  #
  #   class Invoice < Struct.new(:client)
  #     delegate :name, :address, to: :client, prefix: :customer
  #   end
  #
  #   invoice = Invoice.new(john_doe)
  #   invoice.customer_name    # => 'John Doe'
  #   invoice.customer_address # => 'Vimmersvej 13'
  #
  # If the target is +nil+ and does not respond to the delegated method a
  # +NoMethodError+ is raised, as with any other value. Sometimes, however, it
  # makes sense to be robust to that situation and that is the purpose of the
  # <tt>:allow_nil</tt> option: If the target is not +nil+, or it is and
  # responds to the method, everything works as usual. But if it is +nil+ and
  # does not respond to the delegated method, +nil+ is returned.
  #
  #   class User < ActiveRecord::Base
  #     has_one :profile
  #     delegate :age, to: :profile
  #   end
  #
  #   User.new.age # raises NoMethodError: undefined method `age'
  #
  # But if not having a profile yet is fine and should not be an error
  # condition:
  #
  #   class User < ActiveRecord::Base
  #     has_one :profile
  #     delegate :age, to: :profile, allow_nil: true
  #   end
  #
  #   User.new.age # nil
  #
  # Note that if the target is not +nil+ then the call is attempted regardless of the
  # <tt>:allow_nil</tt> option, and thus an exception is still raised if said object
  # does not respond to the method:
  #
  #   class Foo
  #     def initialize(bar)
  #       @bar = bar
  #     end
  #
  #     delegate :name, to: :@bar, allow_nil: true
  #   end
  #
  #   Foo.new("Bar").name # raises NoMethodError: undefined method `name'
  #
  # The target method must be public, otherwise it will raise +NoMethodError+.
  #
  def delegate(*methods)
    options = methods.pop
    unless options.is_a?(Hash) && to = options[:to]
      raise ArgumentError, 'Delegation needs a target. Supply an options hash with a :to key as the last argument (e.g. delegate :hello, to: :greeter).'
    end

    prefix, allow_nil = options.values_at(:prefix, :allow_nil)

    if prefix == true && to =~ /^[^a-z_]/
      raise ArgumentError, 'Can only automatically set the delegation prefix when delegating to a method.'
    end

    method_prefix = \
      if prefix
        "#{prefix == true ? to : prefix}_"
      else
        ''
      end

    file, line = caller(1, 1).first.split(':', 2)
    line = line.to_i

    to = to.to_s
    to = "self.#{to}" if RUBY_RESERVED_WORDS.include?(to)

    methods.each do |method|
      # Attribute writer methods only accept one argument. Makes sure []=
      # methods still accept two arguments.
      definition = (method =~ /[^\]]=$/) ? 'arg' : '*args, &block'

      # The following generated method calls the target exactly once, storing
      # the returned value in a dummy variable.
      #
      # Reason is twofold: On one hand doing less calls is in general better.
      # On the other hand it could be that the target has side-effects,
      # whereas conceptually, from the user point of view, the delegator should
      # be doing one call.
      if allow_nil
        method_def = [
          "def #{method_prefix}#{method}(#{definition})",
          "_ = #{to}",
          "if !_.nil? || nil.respond_to?(:#{method})",
          "  _.#{method}(#{definition})",
          "end",
        "end"
        ].join ';'
      else
        exception = %(raise DelegationError, "#{self}##{method_prefix}#{method} delegated to #{to}.#{method}, but #{to} is nil: \#{self.inspect}")

        method_def = [
          "def #{method_prefix}#{method}(#{definition})",
          " _ = #{to}",
          "  _.#{method}(#{definition})",
          "rescue NoMethodError => e",
          "  if _.nil? && e.name == :#{method}",
          "    #{exception}",
          "  else",
          "    raise",
          "  end",
          "end"
        ].join ';'
      end

      module_eval(method_def, file, line)
    end
  end
end
class Module
  #   deprecate :foo
  #   deprecate bar: 'message'
  #   deprecate :foo, :bar, baz: 'warning!', qux: 'gone!'
  #
  # You can also use custom deprecator instance:
  #
  #   deprecate :foo, deprecator: MyLib::Deprecator.new
  #   deprecate :foo, bar: "warning!", deprecator: MyLib::Deprecator.new
  #
  # \Custom deprecators must respond to <tt>deprecation_warning(deprecated_method_name, message, caller_backtrace)</tt>
  # method where you can implement your custom warning behavior.
  #
  #   class MyLib::Deprecator
  #     def deprecation_warning(deprecated_method_name, message, caller_backtrace = nil)
  #        message = "#{deprecated_method_name} is deprecated and will be removed from MyLibrary | #{message}"
  #        Kernel.warn message
  #     end
  #   end
  def deprecate(*method_names)
    ActiveSupport::Deprecation.deprecate_methods(self, *method_names)
  end
end
require 'active_support/inflector'

class Module
  # Returns the name of the module containing this one.
  #
  #   M::N.parent_name # => "M"
  def parent_name
    if defined? @parent_name
      @parent_name
    else
      @parent_name = name =~ /::[^:]+\Z/ ? $`.freeze : nil
    end
  end

  # Returns the module which contains this one according to its name.
  #
  #   module M
  #     module N
  #     end
  #   end
  #   X = M::N
  #
  #   M::N.parent # => M
  #   X.parent    # => M
  #
  # The parent of top-level and anonymous modules is Object.
  #
  #   M.parent          # => Object
  #   Module.new.parent # => Object
  def parent
    parent_name ? ActiveSupport::Inflector.constantize(parent_name) : Object
  end

  # Returns all the parents of this module according to its name, ordered from
  # nested outwards. The receiver is not contained within the result.
  #
  #   module M
  #     module N
  #     end
  #   end
  #   X = M::N
  #
  #   M.parents    # => [Object]
  #   M::N.parents # => [M, Object]
  #   X.parents    # => [M, Object]
  def parents
    parents = []
    if parent_name
      parts = parent_name.split('::')
      until parts.empty?
        parents << ActiveSupport::Inflector.constantize(parts * '::')
        parts.pop
      end
    end
    parents << Object unless parents.include? Object
    parents
  end

  def local_constants #:nodoc:
    constants(false)
  end
end
require 'active_support/deprecation'

ActiveSupport::Deprecation.warn("This file is deprecated and will be removed in Rails 5.1 with no replacement.")
require 'active_support/core_ext/string/inflections'

#--
# Allows code reuse in the methods below without polluting Module.
#++
module QualifiedConstUtils
  def self.raise_if_absolute(path)
    raise NameError.new("wrong constant name #$&") if path =~ /\A::[^:]+/
  end

  def self.names(path)
    path.split('::')
  end
end

##
# Extends the API for constants to be able to deal with qualified names. Arguments
# are assumed to be relative to the receiver.
#
#--
# Qualified names are required to be relative because we are extending existing
# methods that expect constant names, ie, relative paths of length 1. For example,
# Object.const_get('::String') raises NameError and so does qualified_const_get.
#++
class Module
  def qualified_const_defined?(path, search_parents=true)
    QualifiedConstUtils.raise_if_absolute(path)

    QualifiedConstUtils.names(path).inject(self) do |mod, name|
      return unless mod.const_defined?(name, search_parents)
      mod.const_get(name)
    end
    return true
  end

  def qualified_const_get(path)
    QualifiedConstUtils.raise_if_absolute(path)

    QualifiedConstUtils.names(path).inject(self) do |mod, name|
      mod.const_get(name)
    end
  end

  def qualified_const_set(path, value)
    QualifiedConstUtils.raise_if_absolute(path)

    const_name = path.demodulize
    mod_name = path.deconstantize
    mod = mod_name.empty? ? self : qualified_const_get(mod_name)
    mod.const_set(const_name, value)
  end
end
require 'active_support/core_ext/module/anonymous'
require 'active_support/core_ext/string/inflections'

class Module
  def reachable? #:nodoc:
    !anonymous? && name.safe_constantize.equal?(self)
  end
end
class Module
  # Removes the named method, if it exists.
  def remove_possible_method(method)
    if method_defined?(method) || private_method_defined?(method)
      undef_method(method)
    end
  end

  # Replaces the existing method definition, if there is one, with the passed
  # block as its body.
  def redefine_method(method, &block)
    remove_possible_method(method)
    define_method(method, &block)
  end
end
class NameError
  # Extract the name of the missing constant from the exception message.
  #
  #   begin
  #     HelloWorld
  #   rescue NameError => e
  #     e.missing_name
  #   end
  #   # => "HelloWorld"
  def missing_name
    if /undefined local variable or method/ !~ message
      $1 if /((::)?([A-Z]\w*)(::[A-Z]\w*)*)$/ =~ message
    end
  end

  # Was this exception raised because the given name was missing?
  #
  #   begin
  #     HelloWorld
  #   rescue NameError => e
  #     e.missing_name?("HelloWorld")
  #   end
  #   # => true
  def missing_name?(name)
    if name.is_a? Symbol
      self.name == name
    else
      missing_name == name.to_s
    end
  end
end
require 'active_support/core_ext/numeric/bytes'
require 'active_support/core_ext/numeric/time'
require 'active_support/core_ext/numeric/inquiry'
require 'active_support/core_ext/numeric/conversions'
class Numeric
  KILOBYTE = 1024
  MEGABYTE = KILOBYTE * 1024
  GIGABYTE = MEGABYTE * 1024
  TERABYTE = GIGABYTE * 1024
  PETABYTE = TERABYTE * 1024
  EXABYTE  = PETABYTE * 1024

  # Enables the use of byte calculations and declarations, like 45.bytes + 2.6.megabytes
  #
  #   2.bytes # => 2
  def bytes
    self
  end
  alias :byte :bytes

  # Returns the number of bytes equivalent to the kilobytes provided.
  #
  #   2.kilobytes # => 2048
  def kilobytes
    self * KILOBYTE
  end
  alias :kilobyte :kilobytes

  # Returns the number of bytes equivalent to the megabytes provided.
  #
  #   2.megabytes # => 2_097_152
  def megabytes
    self * MEGABYTE
  end
  alias :megabyte :megabytes

  # Returns the number of bytes equivalent to the gigabytes provided.
  #
  #   2.gigabytes # => 2_147_483_648
  def gigabytes
    self * GIGABYTE
  end
  alias :gigabyte :gigabytes

  # Returns the number of bytes equivalent to the terabytes provided.
  #
  #   2.terabytes # => 2_199_023_255_552
  def terabytes
    self * TERABYTE
  end
  alias :terabyte :terabytes

  # Returns the number of bytes equivalent to the petabytes provided.
  #
  #   2.petabytes # => 2_251_799_813_685_248
  def petabytes
    self * PETABYTE
  end
  alias :petabyte :petabytes

  # Returns the number of bytes equivalent to the exabytes provided.
  #
  #   2.exabytes # => 2_305_843_009_213_693_952
  def exabytes
    self * EXABYTE
  end
  alias :exabyte :exabytes
end
require 'active_support/core_ext/big_decimal/conversions'
require 'active_support/number_helper'

class Numeric

  # Provides options for converting numbers into formatted strings.
  # Options are provided for phone numbers, currency, percentage,
  # precision, positional notation, file size and pretty printing.
  #
  # ==== Options
  #
  # For details on which formats use which options, see ActiveSupport::NumberHelper
  #
  # ==== Examples
  #
  #  Phone Numbers:
  #  5551234.to_s(:phone)                                     # => 555-1234
  #  1235551234.to_s(:phone)                                  # => 123-555-1234
  #  1235551234.to_s(:phone, area_code: true)                 # => (123) 555-1234
  #  1235551234.to_s(:phone, delimiter: ' ')                  # => 123 555 1234
  #  1235551234.to_s(:phone, area_code: true, extension: 555) # => (123) 555-1234 x 555
  #  1235551234.to_s(:phone, country_code: 1)                 # => +1-123-555-1234
  #  1235551234.to_s(:phone, country_code: 1, extension: 1343, delimiter: '.')
  #  # => +1.123.555.1234 x 1343
  #
  #  Currency:
  #  1234567890.50.to_s(:currency)                 # => $1,234,567,890.50
  #  1234567890.506.to_s(:currency)                # => $1,234,567,890.51
  #  1234567890.506.to_s(:currency, precision: 3)  # => $1,234,567,890.506
  #  1234567890.506.to_s(:currency, locale: :fr)   # => 1 234 567 890,51 €
  #  -1234567890.50.to_s(:currency, negative_format: '(%u%n)')
  #  # => ($1,234,567,890.50)
  #  1234567890.50.to_s(:currency, unit: '&pound;', separator: ',', delimiter: '')
  #  # => &pound;1234567890,50
  #  1234567890.50.to_s(:currency, unit: '&pound;', separator: ',', delimiter: '', format: '%n %u')
  #  # => 1234567890,50 &pound;
  #
  #  Percentage:
  #  100.to_s(:percentage)                                  # => 100.000%
  #  100.to_s(:percentage, precision: 0)                    # => 100%
  #  1000.to_s(:percentage, delimiter: '.', separator: ',') # => 1.000,000%
  #  302.24398923423.to_s(:percentage, precision: 5)        # => 302.24399%
  #  1000.to_s(:percentage, locale: :fr)                    # => 1 000,000%
  #  100.to_s(:percentage, format: '%n  %')                 # => 100  %
  #
  #  Delimited:
  #  12345678.to_s(:delimited)                     # => 12,345,678
  #  12345678.05.to_s(:delimited)                  # => 12,345,678.05
  #  12345678.to_s(:delimited, delimiter: '.')     # => 12.345.678
  #  12345678.to_s(:delimited, delimiter: ',')     # => 12,345,678
  #  12345678.05.to_s(:delimited, separator: ' ')  # => 12,345,678 05
  #  12345678.05.to_s(:delimited, locale: :fr)     # => 12 345 678,05
  #  98765432.98.to_s(:delimited, delimiter: ' ', separator: ',')
  #  # => 98 765 432,98
  #
  #  Rounded:
  #  111.2345.to_s(:rounded)                                      # => 111.235
  #  111.2345.to_s(:rounded, precision: 2)                        # => 111.23
  #  13.to_s(:rounded, precision: 5)                              # => 13.00000
  #  389.32314.to_s(:rounded, precision: 0)                       # => 389
  #  111.2345.to_s(:rounded, significant: true)                   # => 111
  #  111.2345.to_s(:rounded, precision: 1, significant: true)     # => 100
  #  13.to_s(:rounded, precision: 5, significant: true)           # => 13.000
  #  111.234.to_s(:rounded, locale: :fr)                          # => 111,234
  #  13.to_s(:rounded, precision: 5, significant: true, strip_insignificant_zeros: true)
  #  # => 13
  #  389.32314.to_s(:rounded, precision: 4, significant: true)    # => 389.3
  #  1111.2345.to_s(:rounded, precision: 2, separator: ',', delimiter: '.')
  #  # => 1.111,23
  #
  #  Human-friendly size in Bytes:
  #  123.to_s(:human_size)                                   # => 123 Bytes
  #  1234.to_s(:human_size)                                  # => 1.21 KB
  #  12345.to_s(:human_size)                                 # => 12.1 KB
  #  1234567.to_s(:human_size)                               # => 1.18 MB
  #  1234567890.to_s(:human_size)                            # => 1.15 GB
  #  1234567890123.to_s(:human_size)                         # => 1.12 TB
  #  1234567.to_s(:human_size, precision: 2)                 # => 1.2 MB
  #  483989.to_s(:human_size, precision: 2)                  # => 470 KB
  #  1234567.to_s(:human_size, precision: 2, separator: ',') # => 1,2 MB
  #  1234567890123.to_s(:human_size, precision: 5)           # => "1.1229 TB"
  #  524288000.to_s(:human_size, precision: 5)               # => "500 MB"
  #
  #  Human-friendly format:
  #  123.to_s(:human)                                       # => "123"
  #  1234.to_s(:human)                                      # => "1.23 Thousand"
  #  12345.to_s(:human)                                     # => "12.3 Thousand"
  #  1234567.to_s(:human)                                   # => "1.23 Million"
  #  1234567890.to_s(:human)                                # => "1.23 Billion"
  #  1234567890123.to_s(:human)                             # => "1.23 Trillion"
  #  1234567890123456.to_s(:human)                          # => "1.23 Quadrillion"
  #  1234567890123456789.to_s(:human)                       # => "1230 Quadrillion"
  #  489939.to_s(:human, precision: 2)                      # => "490 Thousand"
  #  489939.to_s(:human, precision: 4)                      # => "489.9 Thousand"
  #  1234567.to_s(:human, precision: 4,
  #                   significant: false)                   # => "1.2346 Million"
  #  1234567.to_s(:human, precision: 1,
  #                   separator: ',',
  #                   significant: false)                   # => "1,2 Million"
  def to_formatted_s(format = :default, options = {})
    case format
    when :phone
      return ActiveSupport::NumberHelper.number_to_phone(self, options)
    when :currency
      return ActiveSupport::NumberHelper.number_to_currency(self, options)
    when :percentage
      return ActiveSupport::NumberHelper.number_to_percentage(self, options)
    when :delimited
      return ActiveSupport::NumberHelper.number_to_delimited(self, options)
    when :rounded
      return ActiveSupport::NumberHelper.number_to_rounded(self, options)
    when :human
      return ActiveSupport::NumberHelper.number_to_human(self, options)
    when :human_size
      return ActiveSupport::NumberHelper.number_to_human_size(self, options)
    else
      self.to_default_s
    end
  end

  [Fixnum, Bignum].each do |klass|
    klass.class_eval do
      alias_method :to_default_s, :to_s
      def to_s(base_or_format = 10, options = nil)
        if base_or_format.is_a?(Symbol)
          to_formatted_s(base_or_format, options || {})
        else
          to_default_s(base_or_format)
        end
      end
    end
  end

  Float.class_eval do
    alias_method :to_default_s, :to_s
    def to_s(*args)
      if args.empty?
        to_default_s
      else
        to_formatted_s(*args)
      end
    end
  end

end
unless 1.respond_to?(:positive?) # TODO: Remove this file when we drop support to ruby < 2.3
class Numeric
  # Returns true if the number is positive.
  #
  #   1.positive?  # => true
  #   0.positive?  # => false
  #   -1.positive? # => false
  def positive?
    self > 0
  end

  # Returns true if the number is negative.
  #
  #   -1.negative? # => true
  #   0.negative?  # => false
  #   1.negative?  # => false
  def negative?
    self < 0
  end
end

class Complex
  undef :positive?
  undef :negative?
end
end
require 'active_support/duration'
require 'active_support/core_ext/time/calculations'
require 'active_support/core_ext/time/acts_like'
require 'active_support/core_ext/date/calculations'
require 'active_support/core_ext/date/acts_like'

class Numeric
  # Enables the use of time calculations and declarations, like 45.minutes + 2.hours + 4.years.
  #
  # These methods use Time#advance for precise date calculations when using from_now, ago, etc.
  # as well as adding or subtracting their results from a Time object. For example:
  #
  #   # equivalent to Time.current.advance(months: 1)
  #   1.month.from_now
  #
  #   # equivalent to Time.current.advance(years: 2)
  #   2.years.from_now
  #
  #   # equivalent to Time.current.advance(months: 4, years: 5)
  #   (4.months + 5.years).from_now
  def seconds
    ActiveSupport::Duration.new(self, [[:seconds, self]])
  end
  alias :second :seconds

  # Returns a Duration instance matching the number of minutes provided.
  #
  #   2.minutes # => 120 seconds
  def minutes
    ActiveSupport::Duration.new(self * 60, [[:seconds, self * 60]])
  end
  alias :minute :minutes

  # Returns a Duration instance matching the number of hours provided.
  #
  #   2.hours # => 7_200 seconds
  def hours
    ActiveSupport::Duration.new(self * 3600, [[:seconds, self * 3600]])
  end
  alias :hour :hours

  # Returns a Duration instance matching the number of days provided.
  #
  #   2.days # => 2 days
  def days
    ActiveSupport::Duration.new(self * 24.hours, [[:days, self]])
  end
  alias :day :days

  # Returns a Duration instance matching the number of weeks provided.
  #
  #   2.weeks # => 14 days
  def weeks
    ActiveSupport::Duration.new(self * 7.days, [[:days, self * 7]])
  end
  alias :week :weeks

  # Returns a Duration instance matching the number of fortnights provided.
  #
  #   2.fortnights # => 28 days
  def fortnights
    ActiveSupport::Duration.new(self * 2.weeks, [[:days, self * 14]])
  end
  alias :fortnight :fortnights

  # Returns the number of milliseconds equivalent to the seconds provided.
  # Used with the standard time durations, like 1.hour.in_milliseconds --
  # so we can feed them to JavaScript functions like getTime().
  #
  #   2.in_milliseconds # => 2_000
  def in_milliseconds
    self * 1000
  end
end
require 'active_support/core_ext/object/acts_like'
require 'active_support/core_ext/object/blank'
require 'active_support/core_ext/object/duplicable'
require 'active_support/core_ext/object/deep_dup'
require 'active_support/core_ext/object/try'
require 'active_support/core_ext/object/inclusion'

require 'active_support/core_ext/object/conversions'
require 'active_support/core_ext/object/instance_variables'

require 'active_support/core_ext/object/json'
require 'active_support/core_ext/object/to_param'
require 'active_support/core_ext/object/to_query'
require 'active_support/core_ext/object/with_options'
class Object
  # A duck-type assistant method. For example, Active Support extends Date
  # to define an <tt>acts_like_date?</tt> method, and extends Time to define
  # <tt>acts_like_time?</tt>. As a result, we can do <tt>x.acts_like?(:time)</tt> and
  # <tt>x.acts_like?(:date)</tt> to do duck-type-safe comparisons, since classes that
  # we want to act like Time simply need to define an <tt>acts_like_time?</tt> method.
  def acts_like?(duck)
    respond_to? :"acts_like_#{duck}?"
  end
end
# encoding: utf-8

class Object
  # An object is blank if it's false, empty, or a whitespace string.
  # For example, '', '   ', +nil+, [], and {} are all blank.
  #
  # This simplifies
  #
  #   address.nil? || address.empty?
  #
  # to
  #
  #   address.blank?
  #
  # @return [true, false]
  def blank?
    respond_to?(:empty?) ? !!empty? : !self
  end

  # An object is present if it's not blank.
  #
  # @return [true, false]
  def present?
    !blank?
  end

  # Returns the receiver if it's present otherwise returns +nil+.
  # <tt>object.presence</tt> is equivalent to
  #
  #    object.present? ? object : nil
  #
  # For example, something like
  #
  #   state   = params[:state]   if params[:state].present?
  #   country = params[:country] if params[:country].present?
  #   region  = state || country || 'US'
  #
  # becomes
  #
  #   region = params[:state].presence || params[:country].presence || 'US'
  #
  # @return [Object]
  def presence
    self if present?
  end
end

class NilClass
  # +nil+ is blank:
  #
  #   nil.blank? # => true
  #
  # @return [true]
  def blank?
    true
  end
end

class FalseClass
  # +false+ is blank:
  #
  #   false.blank? # => true
  #
  # @return [true]
  def blank?
    true
  end
end

class TrueClass
  # +true+ is not blank:
  #
  #   true.blank? # => false
  #
  # @return [false]
  def blank?
    false
  end
end

class Array
  # An array is blank if it's empty:
  #
  #   [].blank?      # => true
  #   [1,2,3].blank? # => false
  #
  # @return [true, false]
  alias_method :blank?, :empty?
end

class Hash
  # A hash is blank if it's empty:
  #
  #   {}.blank?                # => true
  #   { key: 'value' }.blank?  # => false
  #
  # @return [true, false]
  alias_method :blank?, :empty?
end

class String
  BLANK_RE = /\A[[:space:]]*\z/

  # A string is blank if it's empty or contains whitespaces only:
  #
  #   ''.blank?       # => true
  #   '   '.blank?    # => true
  #   "\t\n\r".blank? # => true
  #   ' blah '.blank? # => false
  #
  # Unicode whitespace is supported:
  #
  #   "\u00a0".blank? # => true
  #
  # @return [true, false]
  def blank?
    BLANK_RE === self
  end
end

class Numeric #:nodoc:
  # No number is blank:
  #
  #   1.blank? # => false
  #   0.blank? # => false
  #
  # @return [false]
  def blank?
    false
  end
end
require 'active_support/core_ext/object/to_param'
require 'active_support/core_ext/object/to_query'
require 'active_support/core_ext/array/conversions'
require 'active_support/core_ext/hash/conversions'
require 'active_support/core_ext/object/duplicable'

class Object
  # Returns a deep copy of object if it's duplicable. If it's
  # not duplicable, returns +self+.
  #
  #   object = Object.new
  #   dup    = object.deep_dup
  #   dup.instance_variable_set(:@a, 1)
  #
  #   object.instance_variable_defined?(:@a) # => false
  #   dup.instance_variable_defined?(:@a)    # => true
  def deep_dup
    duplicable? ? dup : self
  end
end

class Array
  # Returns a deep copy of array.
  #
  #   array = [1, [2, 3]]
  #   dup   = array.deep_dup
  #   dup[1][2] = 4
  #
  #   array[1][2] # => nil
  #   dup[1][2]   # => 4
  def deep_dup
    map(&:deep_dup)
  end
end

class Hash
  # Returns a deep copy of hash.
  #
  #   hash = { a: { b: 'b' } }
  #   dup  = hash.deep_dup
  #   dup[:a][:c] = 'c'
  #
  #   hash[:a][:c] # => nil
  #   dup[:a][:c]  # => "c"
  def deep_dup
    each_with_object(dup) do |(key, value), hash|
      hash.delete(key)
      hash[key.deep_dup] = value.deep_dup
    end
  end
end
#--
# Most objects are cloneable, but not all. For example you can't dup +nil+:
#
#   nil.dup # => TypeError: can't dup NilClass
#
# Classes may signal their instances are not duplicable removing +dup+/+clone+
# or raising exceptions from them. So, to dup an arbitrary object you normally
# use an optimistic approach and are ready to catch an exception, say:
#
#   arbitrary_object.dup rescue object
#
# Rails dups objects in a few critical spots where they are not that arbitrary.
# That rescue is very expensive (like 40 times slower than a predicate), and it
# is often triggered.
#
# That's why we hardcode the following cases and check duplicable? instead of
# using that rescue idiom.
#++
class Object
  # Can you safely dup this object?
  #
  # False for +nil+, +false+, +true+, symbol, number objects;
  # true otherwise.
  def duplicable?
    true
  end
end

class NilClass
  # +nil+ is not duplicable:
  #
  #   nil.duplicable? # => false
  #   nil.dup         # => TypeError: can't dup NilClass
  def duplicable?
    false
  end
end

class FalseClass
  # +false+ is not duplicable:
  #
  #   false.duplicable? # => false
  #   false.dup         # => TypeError: can't dup FalseClass
  def duplicable?
    false
  end
end

class TrueClass
  # +true+ is not duplicable:
  #
  #   true.duplicable? # => false
  #   true.dup         # => TypeError: can't dup TrueClass
  def duplicable?
    false
  end
end

class Symbol
  # Symbols are not duplicable:
  #
  #   :my_symbol.duplicable? # => false
  #   :my_symbol.dup         # => TypeError: can't dup Symbol
  def duplicable?
    false
  end
end

class Numeric
  # Numbers are not duplicable:
  #
  #  3.duplicable? # => false
  #  3.dup         # => TypeError: can't dup Fixnum
  def duplicable?
    false
  end
end

require 'bigdecimal'
class BigDecimal
  def duplicable?
    true
  end
end

class Method
  # Methods are not duplicable:
  #
  #  method(:puts).duplicable? # => false
  #  method(:puts).dup         # => TypeError: allocator undefined for Method
  def duplicable?
    false
  end
end
class Object
  # Returns true if this object is included in the argument. Argument must be
  # any object which responds to +#include?+. Usage:
  #
  #   characters = ["Konata", "Kagami", "Tsukasa"]
  #   "Konata".in?(characters) # => true
  #
  # This will throw an ArgumentError if the argument doesn't respond
  # to +#include?+.
  def in?(another_object)
    another_object.include?(self)
  rescue NoMethodError
    raise ArgumentError.new("The parameter passed to #in? must respond to #include?")
  end

  # Returns the receiver if it's included in the argument otherwise returns +nil+.
  # Argument must be any object which responds to +#include?+. Usage:
  #
  #   params[:bucket_type].presence_in %w( project calendar )
  #
  # This will throw an ArgumentError if the argument doesn't respond to +#include?+.
  #
  # @return [Object]
  def presence_in(another_object)
    self.in?(another_object) ? self : nil
  end
end
class Object
  # Returns a hash with string keys that maps instance variable names without "@" to their
  # corresponding values.
  #
  #   class C
  #     def initialize(x, y)
  #       @x, @y = x, y
  #     end
  #   end
  #
  #   C.new(0, 1).instance_values # => {"x" => 0, "y" => 1}
  def instance_values
    Hash[instance_variables.map { |name| [name[1..-1], instance_variable_get(name)] }]
  end

  # Returns an array of instance variable names as strings including "@".
  #
  #   class C
  #     def initialize(x, y)
  #       @x, @y = x, y
  #     end
  #   end
  #
  #   C.new(0, 1).instance_variable_names # => ["@y", "@x"]
  def instance_variable_names
    instance_variables.map(&:to_s)
  end
end
# Hack to load json gem first so we can overwrite its to_json.
require 'json'
require 'bigdecimal'
require 'active_support/core_ext/big_decimal/conversions' # for #to_s
require 'active_support/core_ext/hash/except'
require 'active_support/core_ext/hash/slice'
require 'active_support/core_ext/object/instance_variables'
require 'time'
require 'active_support/core_ext/time/conversions'
require 'active_support/core_ext/date_time/conversions'
require 'active_support/core_ext/date/conversions'

# The JSON gem adds a few modules to Ruby core classes containing :to_json definition, overwriting
# their default behavior. That said, we need to define the basic to_json method in all of them,
# otherwise they will always use to_json gem implementation, which is backwards incompatible in
# several cases (for instance, the JSON implementation for Hash does not work) with inheritance
# and consequently classes as ActiveSupport::OrderedHash cannot be serialized to json.
#
# On the other hand, we should avoid conflict with ::JSON.{generate,dump}(obj). Unfortunately, the
# JSON gem's encoder relies on its own to_json implementation to encode objects. Since it always
# passes a ::JSON::State object as the only argument to to_json, we can detect that and forward the
# calls to the original to_json method.
#
# It should be noted that when using ::JSON.{generate,dump} directly, ActiveSupport's encoder is
# bypassed completely. This means that as_json won't be invoked and the JSON gem will simply
# ignore any options it does not natively understand. This also means that ::JSON.{generate,dump}
# should give exactly the same results with or without active support.

module ActiveSupport
  module ToJsonWithActiveSupportEncoder # :nodoc:
    def to_json(options = nil)
      if options.is_a?(::JSON::State)
        # Called from JSON.{generate,dump}, forward it to JSON gem's to_json
        super(options)
      else
        # to_json is being invoked directly, use ActiveSupport's encoder
        ActiveSupport::JSON.encode(self, options)
      end
    end
  end
end

[Object, Array, FalseClass, Float, Hash, Integer, NilClass, String, TrueClass, Enumerable].reverse_each do |klass|
  klass.prepend(ActiveSupport::ToJsonWithActiveSupportEncoder)
end

class Object
  def as_json(options = nil) #:nodoc:
    if respond_to?(:to_hash)
      to_hash.as_json(options)
    else
      instance_values.as_json(options)
    end
  end
end

class Struct #:nodoc:
  def as_json(options = nil)
    Hash[members.zip(values)].as_json(options)
  end
end

class TrueClass
  def as_json(options = nil) #:nodoc:
    self
  end
end

class FalseClass
  def as_json(options = nil) #:nodoc:
    self
  end
end

class NilClass
  def as_json(options = nil) #:nodoc:
    self
  end
end

class String
  def as_json(options = nil) #:nodoc:
    self
  end
end

class Symbol
  def as_json(options = nil) #:nodoc:
    to_s
  end
end

class Numeric
  def as_json(options = nil) #:nodoc:
    self
  end
end

class Float
  # Encoding Infinity or NaN to JSON should return "null". The default returns
  # "Infinity" or "NaN" which are not valid JSON.
  def as_json(options = nil) #:nodoc:
    finite? ? self : nil
  end
end

class BigDecimal
  # A BigDecimal would be naturally represented as a JSON number. Most libraries,
  # however, parse non-integer JSON numbers directly as floats. Clients using
  # those libraries would get in general a wrong number and no way to recover
  # other than manually inspecting the string with the JSON code itself.
  #
  # That's why a JSON string is returned. The JSON literal is not numeric, but
  # if the other end knows by contract that the data is supposed to be a
  # BigDecimal, it still has the chance to post-process the string and get the
  # real value.
  def as_json(options = nil) #:nodoc:
    finite? ? to_s : nil
  end
end

class Regexp
  def as_json(options = nil) #:nodoc:
    to_s
  end
end

module Enumerable
  def as_json(options = nil) #:nodoc:
    to_a.as_json(options)
  end
end

class Range
  def as_json(options = nil) #:nodoc:
    to_s
  end
end

class Array
  def as_json(options = nil) #:nodoc:
    map { |v| options ? v.as_json(options.dup) : v.as_json }
  end
end

class Hash
  def as_json(options = nil) #:nodoc:
    # create a subset of the hash by applying :only or :except
    subset = if options
      if attrs = options[:only]
        slice(*Array(attrs))
      elsif attrs = options[:except]
        except(*Array(attrs))
      else
        self
      end
    else
      self
    end

    Hash[subset.map { |k, v| [k.to_s, options ? v.as_json(options.dup) : v.as_json] }]
  end
end

class Time
  def as_json(options = nil) #:nodoc:
    if ActiveSupport::JSON::Encoding.use_standard_json_time_format
      xmlschema(ActiveSupport::JSON::Encoding.time_precision)
    else
      %(#{strftime("%Y/%m/%d %H:%M:%S")} #{formatted_offset(false)})
    end
  end
end

class Date
  def as_json(options = nil) #:nodoc:
    if ActiveSupport::JSON::Encoding.use_standard_json_time_format
      strftime("%Y-%m-%d")
    else
      strftime("%Y/%m/%d")
    end
  end
end

class DateTime
  def as_json(options = nil) #:nodoc:
    if ActiveSupport::JSON::Encoding.use_standard_json_time_format
      xmlschema(ActiveSupport::JSON::Encoding.time_precision)
    else
      strftime('%Y/%m/%d %H:%M:%S %z')
    end
  end
end

class Process::Status #:nodoc:
  def as_json(options = nil)
    { :exitstatus => exitstatus, :pid => pid }
  end
end
require 'active_support/core_ext/object/to_query'
require 'cgi'

class Object
  # Alias of <tt>to_s</tt>.
  def to_param
    to_s
  end

  # Converts an object into a string suitable for use as a URL query string,
  # using the given <tt>key</tt> as the param name.
  def to_query(key)
    "#{CGI.escape(key.to_param)}=#{CGI.escape(to_param.to_s)}"
  end
end

class NilClass
  # Returns +self+.
  def to_param
    self
  end
end

class TrueClass
  # Returns +self+.
  def to_param
    self
  end
end

class FalseClass
  # Returns +self+.
  def to_param
    self
  end
end

class Array
  # Calls <tt>to_param</tt> on all its elements and joins the result with
  # slashes. This is used by <tt>url_for</tt> in Action Pack.
  def to_param
    collect(&:to_param).join '/'
  end

  # Converts an array into a string suitable for use as a URL query string,
  # using the given +key+ as the param name.
  #
  #   ['Rails', 'coding'].to_query('hobbies') # => "hobbies%5B%5D=Rails&hobbies%5B%5D=coding"
  def to_query(key)
    prefix = "#{key}[]"

    if empty?
      nil.to_query(prefix)
    else
      collect { |value| value.to_query(prefix) }.join '&'
    end
  end
end

class Hash
  # Returns a string representation of the receiver suitable for use as a URL
  # query string:
  #
  #   {name: 'David', nationality: 'Danish'}.to_query
  #   # => "name=David&nationality=Danish"
  #
  # An optional namespace can be passed to enclose key names:
  #
  #   {name: 'David', nationality: 'Danish'}.to_query('user')
  #   # => "user%5Bname%5D=David&user%5Bnationality%5D=Danish"
  #
  # The string pairs "key=value" that conform the query string
  # are sorted lexicographically in ascending order.
  #
  # This method is also aliased as +to_param+.
  def to_query(namespace = nil)
    collect do |key, value|
      unless (value.is_a?(Hash) || value.is_a?(Array)) && value.empty?
        value.to_query(namespace ? "#{namespace}[#{key}]" : key)
      end
    end.compact.sort! * '&'
  end

  alias_method :to_param, :to_query
end
require 'delegate'

module ActiveSupport
  module Tryable #:nodoc:
    def try(*a, &b)
      try!(*a, &b) if a.empty? || respond_to?(a.first)
    end

    def try!(*a, &b)
      if a.empty? && block_given?
        if b.arity.zero?
          instance_eval(&b)
        else
          yield self
        end
      else
        public_send(*a, &b)
      end
    end
  end
end

class Object
  include ActiveSupport::Tryable

  ##
  # :method: try
  #
  # :call-seq:
  #   try(*a, &b)
  #
  # Invokes the public method whose name goes as first argument just like
  # +public_send+ does, except that if the receiver does not respond to it the
  # call returns +nil+ rather than raising an exception.
  #
  # This method is defined to be able to write
  #
  #   @person.try(:name)
  #
  # instead of
  #
  #   @person.name if @person
  #
  # +try+ calls can be chained:
  #
  #   @person.try(:spouse).try(:name)
  #
  # instead of
  #
  #   @person.spouse.name if @person && @person.spouse
  #
  # +try+ will also return +nil+ if the receiver does not respond to the method:
  #
  #   @person.try(:non_existing_method) # => nil
  #
  # instead of
  #
  #   @person.non_existing_method if @person.respond_to?(:non_existing_method) # => nil
  #
  # +try+ returns +nil+ when called on +nil+ regardless of whether it responds
  # to the method:
  #
  #   nil.try(:to_i) # => nil, rather than 0
  #
  # Arguments and blocks are forwarded to the method if invoked:
  #
  #   @posts.try(:each_slice, 2) do |a, b|
  #     ...
  #   end
  #
  # The number of arguments in the signature must match. If the object responds
  # to the method the call is attempted and +ArgumentError+ is still raised
  # in case of argument mismatch.
  #
  # If +try+ is called without arguments it yields the receiver to a given
  # block unless it is +nil+:
  #
  #   @person.try do |p|
  #     ...
  #   end
  #
  # You can also call try with a block without accepting an argument, and the block
  # will be instance_eval'ed instead:
  #
  #   @person.try { upcase.truncate(50) }
  #
  # Please also note that +try+ is defined on +Object+. Therefore, it won't work
  # with instances of classes that do not have +Object+ among their ancestors,
  # like direct subclasses of +BasicObject+.

  ##
  # :method: try!
  #
  # :call-seq:
  #   try!(*a, &b)
  #
  # Same as #try, but raises a NoMethodError exception if the receiver is
  # not +nil+ and does not implement the tried method.
  #
  #   "a".try!(:upcase) # => "A"
  #   nil.try!(:upcase) # => nil
  #   123.try!(:upcase) # => NoMethodError: undefined method `upcase' for 123:Fixnum
end

class Delegator
  include ActiveSupport::Tryable

  ##
  # :method: try
  #
  # :call-seq:
  #   try(a*, &b)
  #
  # See Object#try

  ##
  # :method: try!
  #
  # :call-seq:
  #   try!(a*, &b)
  #
  # See Object#try!
end

class NilClass
  # Calling +try+ on +nil+ always returns +nil+.
  # It becomes especially helpful when navigating through associations that may return +nil+.
  #
  #   nil.try(:name) # => nil
  #
  # Without +try+
  #   @person && @person.children.any? && @person.children.first.name
  #
  # With +try+
  #   @person.try(:children).try(:first).try(:name)
  def try(*args)
    nil
  end

  # Calling +try!+ on +nil+ always returns +nil+.
  #
  #   nil.try!(:name) # => nil
  def try!(*args)
    nil
  end
end
require 'active_support/option_merger'

class Object
  # An elegant way to factor duplication out of options passed to a series of
  # method calls. Each method called in the block, with the block variable as
  # the receiver, will have its options merged with the default +options+ hash
  # provided. Each method called on the block variable must take an options
  # hash as its final argument.
  #
  # Without <tt>with_options</tt>, this code contains duplication:
  #
  #   class Account < ActiveRecord::Base
  #     has_many :customers, dependent: :destroy
  #     has_many :products,  dependent: :destroy
  #     has_many :invoices,  dependent: :destroy
  #     has_many :expenses,  dependent: :destroy
  #   end
  #
  # Using <tt>with_options</tt>, we can remove the duplication:
  #
  #   class Account < ActiveRecord::Base
  #     with_options dependent: :destroy do |assoc|
  #       assoc.has_many :customers
  #       assoc.has_many :products
  #       assoc.has_many :invoices
  #       assoc.has_many :expenses
  #     end
  #   end
  #
  # It can also be used with an explicit receiver:
  #
  #   I18n.with_options locale: user.locale, scope: 'newsletter' do |i18n|
  #     subject i18n.t :subject
  #     body    i18n.t :body, user_name: user.name
  #   end
  #
  # When you don't pass an explicit receiver, it executes the whole block
  # in merging options context:
  #
  #   class Account < ActiveRecord::Base
  #     with_options dependent: :destroy do
  #       has_many :customers
  #       has_many :products
  #       has_many :invoices
  #       has_many :expenses
  #     end
  #   end
  #
  # <tt>with_options</tt> can also be nested since the call is forwarded to its receiver.
  #
  # NOTE: Each nesting level will merge inherited defaults in addition to their own.
  #
  #   class Post < ActiveRecord::Base
  #     with_options if: :persisted?, length: { minimum: 50 } do
  #       validates :content, if: -> { content.present? }
  #     end
  #   end
  #
  # The code is equivalent to:
  #
  #   validates :content, length: { minimum: 50 }, if: -> { content.present? }
  #
  # Hence the inherited default for `if` key is ignored.
  #
  def with_options(options, &block)
    option_merger = ActiveSupport::OptionMerger.new(self, options)
    block.arity.zero? ? option_merger.instance_eval(&block) : block.call(option_merger)
  end
end
require 'active_support/core_ext/range/conversions'
require 'active_support/core_ext/range/include_range'
require 'active_support/core_ext/range/overlaps'
require 'active_support/core_ext/range/each'
class Range
  RANGE_FORMATS = {
    :db => Proc.new { |start, stop| "BETWEEN '#{start.to_s(:db)}' AND '#{stop.to_s(:db)}'" }
  }

  # Convert range to a formatted string. See RANGE_FORMATS for predefined formats.
  #
  # This method is aliased to <tt>to_s</tt>.
  #
  #   range = (1..100)           # => 1..100
  #
  #   range.to_formatted_s       # => "1..100"
  #   range.to_s                 # => "1..100"
  #
  #   range.to_formatted_s(:db)  # => "BETWEEN '1' AND '100'"
  #   range.to_s(:db)            # => "BETWEEN '1' AND '100'"
  #
  # == Adding your own range formats to to_formatted_s
  # You can add your own formats to the Range::RANGE_FORMATS hash.
  # Use the format name as the hash key and a Proc instance.
  #
  #   # config/initializers/range_formats.rb
  #   Range::RANGE_FORMATS[:short] = ->(start, stop) { "Between #{start.to_s(:db)} and #{stop.to_s(:db)}" }
  def to_formatted_s(format = :default)
    if formatter = RANGE_FORMATS[format]
      formatter.call(first, last)
    else
      to_default_s
    end
  end

  alias_method :to_default_s, :to_s
  alias_method :to_s, :to_formatted_s
end
module ActiveSupport
  module EachTimeWithZone #:nodoc:
    def each(&block)
      ensure_iteration_allowed
      super
    end

    def step(n = 1, &block)
      ensure_iteration_allowed
      super
    end

    private

      def ensure_iteration_allowed
        raise TypeError, "can't iterate from #{first.class}" if first.is_a?(Time)
      end
  end
end

Range.prepend(ActiveSupport::EachTimeWithZone)
module ActiveSupport
  module IncludeWithRange #:nodoc:
    # Extends the default Range#include? to support range comparisons.
    #  (1..5).include?(1..5) # => true
    #  (1..5).include?(2..3) # => true
    #  (1..5).include?(2..6) # => false
    #
    # The native Range#include? behavior is untouched.
    #  ('a'..'f').include?('c') # => true
    #  (5..9).include?(11) # => false
    def include?(value)
      if value.is_a?(::Range)
        # 1...10 includes 1..9 but it does not include 1..10.
        operator = exclude_end? && !value.exclude_end? ? :< : :<=
        super(value.first) && value.last.send(operator, last)
      else
        super
      end
    end
  end
end

Range.prepend(ActiveSupport::IncludeWithRange)
class Range
  # Compare two ranges and see if they overlap each other
  #  (1..5).overlaps?(4..6) # => true
  #  (1..5).overlaps?(7..9) # => false
  def overlaps?(other)
    cover?(other.first) || other.cover?(first)
  end
end
class Regexp #:nodoc:
  def multiline?
    options & MULTILINE == MULTILINE
  end
end
require 'securerandom'

module SecureRandom
  BASE58_ALPHABET = ('0'..'9').to_a  + ('A'..'Z').to_a + ('a'..'z').to_a - ['0', 'O', 'I', 'l']
  # SecureRandom.base58 generates a random base58 string.
  #
  # The argument _n_ specifies the length, of the random string to be generated.
  #
  # If _n_ is not specified or is nil, 16 is assumed. It may be larger in the future.
  #
  # The result may contain alphanumeric characters except 0, O, I and l
  #
  #   p SecureRandom.base58 #=> "4kUgL2pdQMSCQtjE"
  #   p SecureRandom.base58(24) #=> "77TMHrHJFvFDwodq8w7Ev2m7"
  #
  def self.base58(n = 16)
    SecureRandom.random_bytes(n).unpack("C*").map do |byte|
      idx = byte % 64
      idx = SecureRandom.random_number(58) if idx >= 58
      BASE58_ALPHABET[idx]
    end.join
  end
end
require 'active_support/core_ext/string/conversions'
require 'active_support/core_ext/string/filters'
require 'active_support/core_ext/string/multibyte'
require 'active_support/core_ext/string/starts_ends_with'
require 'active_support/core_ext/string/inflections'
require 'active_support/core_ext/string/access'
require 'active_support/core_ext/string/behavior'
require 'active_support/core_ext/string/output_safety'
require 'active_support/core_ext/string/exclude'
require 'active_support/core_ext/string/strip'
require 'active_support/core_ext/string/inquiry'
require 'active_support/core_ext/string/indent'
require 'active_support/core_ext/string/zones'
class String
  # If you pass a single Fixnum, returns a substring of one character at that
  # position. The first character of the string is at position 0, the next at
  # position 1, and so on. If a range is supplied, a substring containing
  # characters at offsets given by the range is returned. In both cases, if an
  # offset is negative, it is counted from the end of the string. Returns nil
  # if the initial offset falls outside the string. Returns an empty string if
  # the beginning of the range is greater than the end of the string.
  #
  #   str = "hello"
  #   str.at(0)      # => "h"
  #   str.at(1..3)   # => "ell"
  #   str.at(-2)     # => "l"
  #   str.at(-2..-1) # => "lo"
  #   str.at(5)      # => nil
  #   str.at(5..-1)  # => ""
  #
  # If a Regexp is given, the matching portion of the string is returned.
  # If a String is given, that given string is returned if it occurs in
  # the string. In both cases, nil is returned if there is no match.
  #
  #   str = "hello"
  #   str.at(/lo/) # => "lo"
  #   str.at(/ol/) # => nil
  #   str.at("lo") # => "lo"
  #   str.at("ol") # => nil
  def at(position)
    self[position]
  end

  # Returns a substring from the given position to the end of the string.
  # If the position is negative, it is counted from the end of the string.
  #
  #   str = "hello"
  #   str.from(0)  # => "hello"
  #   str.from(3)  # => "lo"
  #   str.from(-2) # => "lo"
  #
  # You can mix it with +to+ method and do fun things like:
  #
  #   str = "hello"
  #   str.from(0).to(-1) # => "hello"
  #   str.from(1).to(-2) # => "ell"
  def from(position)
    self[position..-1]
  end

  # Returns a substring from the beginning of the string to the given position.
  # If the position is negative, it is counted from the end of the string.
  #
  #   str = "hello"
  #   str.to(0)  # => "h"
  #   str.to(3)  # => "hell"
  #   str.to(-2) # => "hell"
  #
  # You can mix it with +from+ method and do fun things like:
  #
  #   str = "hello"
  #   str.from(0).to(-1) # => "hello"
  #   str.from(1).to(-2) # => "ell"
  def to(position)
    self[0..position]
  end

  # Returns the first character. If a limit is supplied, returns a substring
  # from the beginning of the string until it reaches the limit value. If the
  # given limit is greater than or equal to the string length, returns a copy of self.
  #
  #   str = "hello"
  #   str.first    # => "h"
  #   str.first(1) # => "h"
  #   str.first(2) # => "he"
  #   str.first(0) # => ""
  #   str.first(6) # => "hello"
  def first(limit = 1)
    if limit == 0
      ''
    elsif limit >= size
      self.dup
    else
      to(limit - 1)
    end
  end

  # Returns the last character of the string. If a limit is supplied, returns a substring
  # from the end of the string until it reaches the limit value (counting backwards). If
  # the given limit is greater than or equal to the string length, returns a copy of self.
  #
  #   str = "hello"
  #   str.last    # => "o"
  #   str.last(1) # => "o"
  #   str.last(2) # => "lo"
  #   str.last(0) # => ""
  #   str.last(6) # => "hello"
  def last(limit = 1)
    if limit == 0
      ''
    elsif limit >= size
      self.dup
    else
      from(-limit)
    end
  end
end
class String
  # Enables more predictable duck-typing on String-like classes. See <tt>Object#acts_like?</tt>.
  def acts_like_string?
    true
  end
end
require 'date'
require 'active_support/core_ext/time/calculations'

class String
  # Converts a string to a Time value.
  # The +form+ can be either :utc or :local (default :local).
  #
  # The time is parsed using Time.parse method.
  # If +form+ is :local, then the time is in the system timezone.
  # If the date part is missing then the current date is used and if
  # the time part is missing then it is assumed to be 00:00:00.
  #
  #   "13-12-2012".to_time               # => 2012-12-13 00:00:00 +0100
  #   "06:12".to_time                    # => 2012-12-13 06:12:00 +0100
  #   "2012-12-13 06:12".to_time         # => 2012-12-13 06:12:00 +0100
  #   "2012-12-13T06:12".to_time         # => 2012-12-13 06:12:00 +0100
  #   "2012-12-13T06:12".to_time(:utc)   # => 2012-12-13 05:12:00 UTC
  #   "12/13/2012".to_time               # => ArgumentError: argument out of range
  def to_time(form = :local)
    parts = Date._parse(self, false)
    return if parts.empty?

    now = Time.now
    time = Time.new(
      parts.fetch(:year, now.year),
      parts.fetch(:mon, now.month),
      parts.fetch(:mday, now.day),
      parts.fetch(:hour, 0),
      parts.fetch(:min, 0),
      parts.fetch(:sec, 0) + parts.fetch(:sec_fraction, 0),
      parts.fetch(:offset, form == :utc ? 0 : nil)
    )

    form == :utc ? time.utc : time.getlocal
  end

  # Converts a string to a Date value.
  #
  #   "1-1-2012".to_date   # => Sun, 01 Jan 2012
  #   "01/01/2012".to_date # => Sun, 01 Jan 2012
  #   "2012-12-13".to_date # => Thu, 13 Dec 2012
  #   "12/13/2012".to_date # => ArgumentError: invalid date
  def to_date
    ::Date.parse(self, false) unless blank?
  end

  # Converts a string to a DateTime value.
  #
  #   "1-1-2012".to_datetime            # => Sun, 01 Jan 2012 00:00:00 +0000
  #   "01/01/2012 23:59:59".to_datetime # => Sun, 01 Jan 2012 23:59:59 +0000
  #   "2012-12-13 12:50".to_datetime    # => Thu, 13 Dec 2012 12:50:00 +0000
  #   "12/13/2012".to_datetime          # => ArgumentError: invalid date
  def to_datetime
    ::DateTime.parse(self, false) unless blank?
  end
end
class String
  # The inverse of <tt>String#include?</tt>. Returns true if the string
  # does not include the other string.
  #
  #   "hello".exclude? "lo" # => false
  #   "hello".exclude? "ol" # => true
  #   "hello".exclude? ?h   # => false
  def exclude?(string)
    !include?(string)
  end
end
class String
  # Returns the string, first removing all whitespace on both ends of
  # the string, and then changing remaining consecutive whitespace
  # groups into one space each.
  #
  # Note that it handles both ASCII and Unicode whitespace.
  #
  #   %{ Multi-line
  #      string }.squish                   # => "Multi-line string"
  #   " foo   bar    \n   \t   boo".squish # => "foo bar boo"
  def squish
    dup.squish!
  end

  # Performs a destructive squish. See String#squish.
  #   str = " foo   bar    \n   \t   boo"
  #   str.squish!                         # => "foo bar boo"
  #   str                                 # => "foo bar boo"
  def squish!
    gsub!(/[[:space:]]+/, ' ')
    strip!
    self
  end

  # Returns a new string with all occurrences of the patterns removed.
  #   str = "foo bar test"
  #   str.remove(" test")                 # => "foo bar"
  #   str.remove(" test", /bar/)          # => "foo "
  #   str                                 # => "foo bar test"
  def remove(*patterns)
    dup.remove!(*patterns)
  end

  # Alters the string by removing all occurrences of the patterns.
  #   str = "foo bar test"
  #   str.remove!(" test", /bar/)         # => "foo "
  #   str                                 # => "foo "
  def remove!(*patterns)
    patterns.each do |pattern|
      gsub! pattern, ""
    end

    self
  end

  # Truncates a given +text+ after a given <tt>length</tt> if +text+ is longer than <tt>length</tt>:
  #
  #   'Once upon a time in a world far far away'.truncate(27)
  #   # => "Once upon a time in a wo..."
  #
  # Pass a string or regexp <tt>:separator</tt> to truncate +text+ at a natural break:
  #
  #   'Once upon a time in a world far far away'.truncate(27, separator: ' ')
  #   # => "Once upon a time in a..."
  #
  #   'Once upon a time in a world far far away'.truncate(27, separator: /\s/)
  #   # => "Once upon a time in a..."
  #
  # The last characters will be replaced with the <tt>:omission</tt> string (defaults to "...")
  # for a total length not exceeding <tt>length</tt>:
  #
  #   'And they found that many people were sleeping better.'.truncate(25, omission: '... (continued)')
  #   # => "And they f... (continued)"
  def truncate(truncate_at, options = {})
    return dup unless length > truncate_at

    omission = options[:omission] || '...'
    length_with_room_for_omission = truncate_at - omission.length
    stop = \
      if options[:separator]
        rindex(options[:separator], length_with_room_for_omission) || length_with_room_for_omission
      else
        length_with_room_for_omission
      end

    "#{self[0, stop]}#{omission}"
  end

  # Truncates a given +text+ after a given number of words (<tt>words_count</tt>):
  #
  #   'Once upon a time in a world far far away'.truncate_words(4)
  #   # => "Once upon a time..."
  #
  # Pass a string or regexp <tt>:separator</tt> to specify a different separator of words:
  #
  #   'Once<br>upon<br>a<br>time<br>in<br>a<br>world'.truncate_words(5, separator: '<br>')
  #   # => "Once<br>upon<br>a<br>time<br>in..."
  #
  # The last characters will be replaced with the <tt>:omission</tt> string (defaults to "..."):
  #
  #   'And they found that many people were sleeping better.'.truncate_words(5, omission: '... (continued)')
  #   # => "And they found that many... (continued)"
  def truncate_words(words_count, options = {})
    sep = options[:separator] || /\s+/
    sep = Regexp.escape(sep.to_s) unless Regexp === sep
    if self =~ /\A((?>.+?#{sep}){#{words_count - 1}}.+?)#{sep}.*/m
      $1 + (options[:omission] || '...')
    else
      dup
    end
  end
end
class String
  # Same as +indent+, except it indents the receiver in-place.
  #
  # Returns the indented string, or +nil+ if there was nothing to indent.
  def indent!(amount, indent_string=nil, indent_empty_lines=false)
    indent_string = indent_string || self[/^[ \t]/] || ' '
    re = indent_empty_lines ? /^/ : /^(?!$)/
    gsub!(re, indent_string * amount)
  end

  # Indents the lines in the receiver:
  #
  #   <<EOS.indent(2)
  #   def some_method
  #     some_code
  #   end
  #   EOS
  #   # =>
  #     def some_method
  #       some_code
  #     end
  #
  # The second argument, +indent_string+, specifies which indent string to
  # use. The default is +nil+, which tells the method to make a guess by
  # peeking at the first indented line, and fallback to a space if there is
  # none.
  #
  #   "  foo".indent(2)        # => "    foo"
  #   "foo\n\t\tbar".indent(2) # => "\t\tfoo\n\t\t\t\tbar"
  #   "foo".indent(2, "\t")    # => "\t\tfoo"
  #
  # While +indent_string+ is typically one space or tab, it may be any string.
  #
  # The third argument, +indent_empty_lines+, is a flag that says whether
  # empty lines should be indented. Default is false.
  #
  #   "foo\n\nbar".indent(2)            # => "  foo\n\n  bar"
  #   "foo\n\nbar".indent(2, nil, true) # => "  foo\n  \n  bar"
  #
  def indent(amount, indent_string=nil, indent_empty_lines=false)
    dup.tap {|_| _.indent!(amount, indent_string, indent_empty_lines)}
  end
end
require 'active_support/inflector/methods'
require 'active_support/inflector/transliterate'

# String inflections define new methods on the String class to transform names for different purposes.
# For instance, you can figure out the name of a table from the name of a class.
#
#   'ScaleScore'.tableize # => "scale_scores"
#
class String
  # Returns the plural form of the word in the string.
  #
  # If the optional parameter +count+ is specified,
  # the singular form will be returned if <tt>count == 1</tt>.
  # For any other value of +count+ the plural will be returned.
  #
  # If the optional parameter +locale+ is specified,
  # the word will be pluralized as a word of that language.
  # By default, this parameter is set to <tt>:en</tt>.
  # You must define your own inflection rules for languages other than English.
  #
  #   'post'.pluralize             # => "posts"
  #   'octopus'.pluralize          # => "octopi"
  #   'sheep'.pluralize            # => "sheep"
  #   'words'.pluralize            # => "words"
  #   'the blue mailman'.pluralize # => "the blue mailmen"
  #   'CamelOctopus'.pluralize     # => "CamelOctopi"
  #   'apple'.pluralize(1)         # => "apple"
  #   'apple'.pluralize(2)         # => "apples"
  #   'ley'.pluralize(:es)         # => "leyes"
  #   'ley'.pluralize(1, :es)      # => "ley"
  def pluralize(count = nil, locale = :en)
    locale = count if count.is_a?(Symbol)
    if count == 1
      self.dup
    else
      ActiveSupport::Inflector.pluralize(self, locale)
    end
  end

  # The reverse of +pluralize+, returns the singular form of a word in a string.
  #
  # If the optional parameter +locale+ is specified,
  # the word will be singularized as a word of that language.
  # By default, this parameter is set to <tt>:en</tt>.
  # You must define your own inflection rules for languages other than English.
  #
  #   'posts'.singularize            # => "post"
  #   'octopi'.singularize           # => "octopus"
  #   'sheep'.singularize            # => "sheep"
  #   'word'.singularize             # => "word"
  #   'the blue mailmen'.singularize # => "the blue mailman"
  #   'CamelOctopi'.singularize      # => "CamelOctopus"
  #   'leyes'.singularize(:es)       # => "ley"
  def singularize(locale = :en)
    ActiveSupport::Inflector.singularize(self, locale)
  end

  # +constantize+ tries to find a declared constant with the name specified
  # in the string. It raises a NameError when the name is not in CamelCase
  # or is not initialized.  See ActiveSupport::Inflector.constantize
  #
  #   'Module'.constantize  # => Module
  #   'Class'.constantize   # => Class
  #   'blargle'.constantize # => NameError: wrong constant name blargle
  def constantize
    ActiveSupport::Inflector.constantize(self)
  end

  # +safe_constantize+ tries to find a declared constant with the name specified
  # in the string. It returns nil when the name is not in CamelCase
  # or is not initialized.  See ActiveSupport::Inflector.safe_constantize
  #
  #   'Module'.safe_constantize  # => Module
  #   'Class'.safe_constantize   # => Class
  #   'blargle'.safe_constantize # => nil
  def safe_constantize
    ActiveSupport::Inflector.safe_constantize(self)
  end

  # By default, +camelize+ converts strings to UpperCamelCase. If the argument to camelize
  # is set to <tt>:lower</tt> then camelize produces lowerCamelCase.
  #
  # +camelize+ will also convert '/' to '::' which is useful for converting paths to namespaces.
  #
  #   'active_record'.camelize                # => "ActiveRecord"
  #   'active_record'.camelize(:lower)        # => "activeRecord"
  #   'active_record/errors'.camelize         # => "ActiveRecord::Errors"
  #   'active_record/errors'.camelize(:lower) # => "activeRecord::Errors"
  def camelize(first_letter = :upper)
    case first_letter
    when :upper
      ActiveSupport::Inflector.camelize(self, true)
    when :lower
      ActiveSupport::Inflector.camelize(self, false)
    end
  end
  alias_method :camelcase, :camelize

  # Capitalizes all the words and replaces some characters in the string to create
  # a nicer looking title. +titleize+ is meant for creating pretty output. It is not
  # used in the Rails internals.
  #
  # +titleize+ is also aliased as +titlecase+.
  #
  #   'man from the boondocks'.titleize # => "Man From The Boondocks"
  #   'x-men: the last stand'.titleize  # => "X Men: The Last Stand"
  def titleize
    ActiveSupport::Inflector.titleize(self)
  end
  alias_method :titlecase, :titleize

  # The reverse of +camelize+. Makes an underscored, lowercase form from the expression in the string.
  #
  # +underscore+ will also change '::' to '/' to convert namespaces to paths.
  #
  #   'ActiveModel'.underscore         # => "active_model"
  #   'ActiveModel::Errors'.underscore # => "active_model/errors"
  def underscore
    ActiveSupport::Inflector.underscore(self)
  end

  # Replaces underscores with dashes in the string.
  #
  #   'puni_puni'.dasherize # => "puni-puni"
  def dasherize
    ActiveSupport::Inflector.dasherize(self)
  end

  # Removes the module part from the constant expression in the string.
  #
  #   'ActiveRecord::CoreExtensions::String::Inflections'.demodulize # => "Inflections"
  #   'Inflections'.demodulize                                       # => "Inflections"
  #   '::Inflections'.demodulize                                     # => "Inflections"
  #   ''.demodulize                                                  # => ''
  #
  # See also +deconstantize+.
  def demodulize
    ActiveSupport::Inflector.demodulize(self)
  end

  # Removes the rightmost segment from the constant expression in the string.
  #
  #   'Net::HTTP'.deconstantize   # => "Net"
  #   '::Net::HTTP'.deconstantize # => "::Net"
  #   'String'.deconstantize      # => ""
  #   '::String'.deconstantize    # => ""
  #   ''.deconstantize            # => ""
  #
  # See also +demodulize+.
  def deconstantize
    ActiveSupport::Inflector.deconstantize(self)
  end

  # Replaces special characters in a string so that it may be used as part of a 'pretty' URL.
  #
  #   class Person
  #     def to_param
  #       "#{id}-#{name.parameterize}"
  #     end
  #   end
  #
  #   @person = Person.find(1)
  #   # => #<Person id: 1, name: "Donald E. Knuth">
  #
  #   <%= link_to(@person.name, person_path) %>
  #   # => <a href="/person/1-donald-e-knuth">Donald E. Knuth</a>
  def parameterize(sep = '-')
    ActiveSupport::Inflector.parameterize(self, sep)
  end

  # Creates the name of a table like Rails does for models to table names. This method
  # uses the +pluralize+ method on the last word in the string.
  #
  #   'RawScaledScorer'.tableize # => "raw_scaled_scorers"
  #   'egg_and_ham'.tableize     # => "egg_and_hams"
  #   'fancyCategory'.tableize   # => "fancy_categories"
  def tableize
    ActiveSupport::Inflector.tableize(self)
  end

  # Creates a class name from a plural table name like Rails does for table names to models.
  # Note that this returns a string and not a class. (To convert to an actual class
  # follow +classify+ with +constantize+.)
  #
  #   'egg_and_hams'.classify # => "EggAndHam"
  #   'posts'.classify        # => "Post"
  def classify
    ActiveSupport::Inflector.classify(self)
  end

  # Capitalizes the first word, turns underscores into spaces, and strips a
  # trailing '_id' if present.
  # Like +titleize+, this is meant for creating pretty output.
  #
  # The capitalization of the first word can be turned off by setting the
  # optional parameter +capitalize+ to false.
  # By default, this parameter is true.
  #
  #   'employee_salary'.humanize              # => "Employee salary"
  #   'author_id'.humanize                    # => "Author"
  #   'author_id'.humanize(capitalize: false) # => "author"
  #   '_id'.humanize                          # => "Id"
  def humanize(options = {})
    ActiveSupport::Inflector.humanize(self, options)
  end

  # Creates a foreign key name from a class name.
  # +separate_class_name_and_id_with_underscore+ sets whether
  # the method should put '_' between the name and 'id'.
  #
  #   'Message'.foreign_key        # => "message_id"
  #   'Message'.foreign_key(false) # => "messageid"
  #   'Admin::Post'.foreign_key    # => "post_id"
  def foreign_key(separate_class_name_and_id_with_underscore = true)
    ActiveSupport::Inflector.foreign_key(self, separate_class_name_and_id_with_underscore)
  end
end
require 'active_support/string_inquirer'

class String
  # Wraps the current string in the <tt>ActiveSupport::StringInquirer</tt> class,
  # which gives you a prettier way to test for equality.
  #
  #   env = 'production'.inquiry
  #   env.production?  # => true
  #   env.development? # => false
  def inquiry
    ActiveSupport::StringInquirer.new(self)
  end
end
require 'active_support/multibyte'

class String
  # == Multibyte proxy
  #
  # +mb_chars+ is a multibyte safe proxy for string methods.
  #
  # It creates and returns an instance of the ActiveSupport::Multibyte::Chars class which
  # encapsulates the original string. A Unicode safe version of all the String methods are defined on this proxy
  # class. If the proxy class doesn't respond to a certain method, it's forwarded to the encapsulated string.
  #
  #   name = 'Claus Müller'
  #   name.reverse # => "rell??M sualC"
  #   name.length  # => 13
  #
  #   name.mb_chars.reverse.to_s # => "rellüM sualC"
  #   name.mb_chars.length       # => 12
  #
  # == Method chaining
  #
  # All the methods on the Chars proxy which normally return a string will return a Chars object. This allows
  # method chaining on the result of any of these methods.
  #
  #   name.mb_chars.reverse.length # => 12
  #
  # == Interoperability and configuration
  #
  # The Chars object tries to be as interchangeable with String objects as possible: sorting and comparing between
  # String and Char work like expected. The bang! methods change the internal string representation in the Chars
  # object. Interoperability problems can be resolved easily with a +to_s+ call.
  #
  # For more information about the methods defined on the Chars proxy see ActiveSupport::Multibyte::Chars. For
  # information about how to change the default Multibyte behavior see ActiveSupport::Multibyte.
  def mb_chars
    ActiveSupport::Multibyte.proxy_class.new(self)
  end

  # Returns +true+ if string has utf_8 encoding.
  #
  #   utf_8_str = "some string".encode "UTF-8"
  #   iso_str = "some string".encode "ISO-8859-1"
  #
  #   utf_8_str.is_utf8? # => true
  #   iso_str.is_utf8?   # => false
  def is_utf8?
    case encoding
    when Encoding::UTF_8
      valid_encoding?
    when Encoding::ASCII_8BIT, Encoding::US_ASCII
      dup.force_encoding(Encoding::UTF_8).valid_encoding?
    else
      false
    end
  end
end
require 'erb'
require 'active_support/core_ext/kernel/singleton_class'

class ERB
  module Util
    HTML_ESCAPE = { '&' => '&amp;',  '>' => '&gt;',   '<' => '&lt;', '"' => '&quot;', "'" => '&#39;' }
    JSON_ESCAPE = { '&' => '\u0026', '>' => '\u003e', '<' => '\u003c', "\u2028" => '\u2028', "\u2029" => '\u2029' }
    HTML_ESCAPE_REGEXP = /[&"'><]/
    HTML_ESCAPE_ONCE_REGEXP = /["><']|&(?!([a-zA-Z]+|(#\d+)|(#[xX][\dA-Fa-f]+));)/
    JSON_ESCAPE_REGEXP = /[\u2028\u2029&><]/u

    # A utility method for escaping HTML tag characters.
    # This method is also aliased as <tt>h</tt>.
    #
    # In your ERB templates, use this method to escape any unsafe content. For example:
    #   <%= h @person.name %>
    #
    #   puts html_escape('is a > 0 & a < 10?')
    #   # => is a &gt; 0 &amp; a &lt; 10?
    def html_escape(s)
      unwrapped_html_escape(s).html_safe
    end

    # Aliasing twice issues a warning "discarding old...". Remove first to avoid it.
    remove_method(:h)
    alias h html_escape

    module_function :h

    singleton_class.send(:remove_method, :html_escape)
    module_function :html_escape

    # HTML escapes strings but doesn't wrap them with an ActiveSupport::SafeBuffer.
    # This method is not for public consumption! Seriously!
    def unwrapped_html_escape(s) # :nodoc:
      s = s.to_s
      if s.html_safe?
        s
      else
        s.gsub(HTML_ESCAPE_REGEXP, HTML_ESCAPE)
      end
    end
    module_function :unwrapped_html_escape

    # A utility method for escaping HTML without affecting existing escaped entities.
    #
    #   html_escape_once('1 < 2 &amp; 3')
    #   # => "1 &lt; 2 &amp; 3"
    #
    #   html_escape_once('&lt;&lt; Accept & Checkout')
    #   # => "&lt;&lt; Accept &amp; Checkout"
    def html_escape_once(s)
      result = s.to_s.gsub(HTML_ESCAPE_ONCE_REGEXP, HTML_ESCAPE)
      s.html_safe? ? result.html_safe : result
    end

    module_function :html_escape_once

    # A utility method for escaping HTML entities in JSON strings. Specifically, the
    # &, > and < characters are replaced with their equivalent unicode escaped form -
    # \u0026, \u003e, and \u003c. The Unicode sequences \u2028 and \u2029 are also
    # escaped as they are treated as newline characters in some JavaScript engines.
    # These sequences have identical meaning as the original characters inside the
    # context of a JSON string, so assuming the input is a valid and well-formed
    # JSON value, the output will have equivalent meaning when parsed:
    #
    #   json = JSON.generate({ name: "</script><script>alert('PWNED!!!')</script>"})
    #   # => "{\"name\":\"</script><script>alert('PWNED!!!')</script>\"}"
    #
    #   json_escape(json)
    #   # => "{\"name\":\"\\u003C/script\\u003E\\u003Cscript\\u003Ealert('PWNED!!!')\\u003C/script\\u003E\"}"
    #
    #   JSON.parse(json) == JSON.parse(json_escape(json))
    #   # => true
    #
    # The intended use case for this method is to escape JSON strings before including
    # them inside a script tag to avoid XSS vulnerability:
    #
    #   <script>
    #     var currentUser = <%= raw json_escape(current_user.to_json) %>;
    #   </script>
    #
    # It is necessary to +raw+ the result of +json_escape+, so that quotation marks
    # don't get converted to <tt>&quot;</tt> entities. +json_escape+ doesn't
    # automatically flag the result as HTML safe, since the raw value is unsafe to
    # use inside HTML attributes.
    #
    # If your JSON is being used downstream for insertion into the DOM, be aware of
    # whether or not it is being inserted via +html()+. Most JQuery plugins do this.
    # If that is the case, be sure to +html_escape+ or +sanitize+ any user-generated
    # content returned by your JSON.
    #
    # If you need to output JSON elsewhere in your HTML, you can just do something
    # like this, as any unsafe characters (including quotation marks) will be
    # automatically escaped for you:
    #
    #   <div data-user-info="<%= current_user.to_json %>">...</div>
    #
    # WARNING: this helper only works with valid JSON. Using this on non-JSON values
    # will open up serious XSS vulnerabilities. For example, if you replace the
    # +current_user.to_json+ in the example above with user input instead, the browser
    # will happily eval() that string as JavaScript.
    #
    # The escaping performed in this method is identical to those performed in the
    # Active Support JSON encoder when +ActiveSupport.escape_html_entities_in_json+ is
    # set to true. Because this transformation is idempotent, this helper can be
    # applied even if +ActiveSupport.escape_html_entities_in_json+ is already true.
    #
    # Therefore, when you are unsure if +ActiveSupport.escape_html_entities_in_json+
    # is enabled, or if you are unsure where your JSON string originated from, it
    # is recommended that you always apply this helper (other libraries, such as the
    # JSON gem, do not provide this kind of protection by default; also some gems
    # might override +to_json+ to bypass Active Support's encoder).
    def json_escape(s)
      result = s.to_s.gsub(JSON_ESCAPE_REGEXP, JSON_ESCAPE)
      s.html_safe? ? result.html_safe : result
    end

    module_function :json_escape
  end
end

class Object
  def html_safe?
    false
  end
end

class Numeric
  def html_safe?
    true
  end
end

module ActiveSupport #:nodoc:
  class SafeBuffer < String
    UNSAFE_STRING_METHODS = %w(
      capitalize chomp chop delete downcase gsub lstrip next reverse rstrip
      slice squeeze strip sub succ swapcase tr tr_s upcase
    )

    alias_method :original_concat, :concat
    private :original_concat

    class SafeConcatError < StandardError
      def initialize
        super 'Could not concatenate to the buffer because it is not html safe.'
      end
    end

    def [](*args)
      if args.size < 2
        super
      else
        if html_safe?
          new_safe_buffer = super

          if new_safe_buffer
            new_safe_buffer.instance_variable_set :@html_safe, true
          end

          new_safe_buffer
        else
          to_str[*args]
        end
      end
    end

    def safe_concat(value)
      raise SafeConcatError unless html_safe?
      original_concat(value)
    end

    def initialize(*)
      @html_safe = true
      super
    end

    def initialize_copy(other)
      super
      @html_safe = other.html_safe?
    end

    def clone_empty
      self[0, 0]
    end

    def concat(value)
      super(html_escape_interpolated_argument(value))
    end
    alias << concat

    def prepend(value)
      super(html_escape_interpolated_argument(value))
    end

    def +(other)
      dup.concat(other)
    end

    def %(args)
      case args
      when Hash
        escaped_args = Hash[args.map { |k,arg| [k, html_escape_interpolated_argument(arg)] }]
      else
        escaped_args = Array(args).map { |arg| html_escape_interpolated_argument(arg) }
      end

      self.class.new(super(escaped_args))
    end

    def html_safe?
      defined?(@html_safe) && @html_safe
    end

    def to_s
      self
    end

    def to_param
      to_str
    end

    def encode_with(coder)
      coder.represent_object nil, to_str
    end

    UNSAFE_STRING_METHODS.each do |unsafe_method|
      if unsafe_method.respond_to?(unsafe_method)
        class_eval <<-EOT, __FILE__, __LINE__ + 1
          def #{unsafe_method}(*args, &block)       # def capitalize(*args, &block)
            to_str.#{unsafe_method}(*args, &block)  #   to_str.capitalize(*args, &block)
          end                                       # end

          def #{unsafe_method}!(*args)              # def capitalize!(*args)
            @html_safe = false                      #   @html_safe = false
            super                                   #   super
          end                                       # end
        EOT
      end
    end

    private

    def html_escape_interpolated_argument(arg)
      (!html_safe? || arg.html_safe?) ? arg :
        arg.to_s.gsub(ERB::Util::HTML_ESCAPE_REGEXP, ERB::Util::HTML_ESCAPE)
    end
  end
end

class String
  # Marks a string as trusted safe. It will be inserted into HTML with no
  # additional escaping performed. It is your responsibilty to ensure that the
  # string contains no malicious content. This method is equivalent to the
  # `raw` helper in views. It is recommended that you use `sanitize` instead of
  # this method. It should never be called on user input.
  def html_safe
    ActiveSupport::SafeBuffer.new(self)
  end
end
class String
  alias_method :starts_with?, :start_with?
  alias_method :ends_with?, :end_with?
end
require 'active_support/core_ext/object/try'

class String
  # Strips indentation in heredocs.
  #
  # For example in
  #
  #   if options[:usage]
  #     puts <<-USAGE.strip_heredoc
  #       This command does such and such.
  #
  #       Supported options are:
  #         -h         This message
  #         ...
  #     USAGE
  #   end
  #
  # the user would see the usage message aligned against the left margin.
  #
  # Technically, it looks for the least indented line in the whole string, and removes
  # that amount of leading whitespace.
  def strip_heredoc
    indent = scan(/^[ \t]*(?=\S)/).min.try(:size) || 0
    gsub(/^[ \t]{#{indent}}/, '')
  end
end
require 'active_support/core_ext/string/conversions'
require 'active_support/core_ext/time/zones'

class String
  # Converts String to a TimeWithZone in the current zone if Time.zone or Time.zone_default
  # is set, otherwise converts String to a Time via String#to_time
  def in_time_zone(zone = ::Time.zone)
    if zone
      ::Time.find_zone!(zone).parse(self)
    else
      to_time
    end
  end
end
require 'active_support/deprecation'

ActiveSupport::Deprecation.warn("This file is deprecated and will be removed in Rails 5.1 with no replacement.")
require 'active_support/core_ext/time/acts_like'
require 'active_support/core_ext/time/calculations'
require 'active_support/core_ext/time/conversions'
require 'active_support/core_ext/time/zones'
require 'active_support/core_ext/object/acts_like'

class Time
  # Duck-types as a Time-like class. See Object#acts_like?.
  def acts_like_time?
    true
  end
end
require 'active_support/duration'
require 'active_support/core_ext/time/conversions'
require 'active_support/time_with_zone'
require 'active_support/core_ext/time/zones'
require 'active_support/core_ext/date_and_time/calculations'

class Time
  include DateAndTime::Calculations

  COMMON_YEAR_DAYS_IN_MONTH = [nil, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  class << self
    # Overriding case equality method so that it returns true for ActiveSupport::TimeWithZone instances
    def ===(other)
      super || (self == Time && other.is_a?(ActiveSupport::TimeWithZone))
    end

    # Return the number of days in the given month.
    # If no year is specified, it will use the current year.
    def days_in_month(month, year = now.year)
      if month == 2 && ::Date.gregorian_leap?(year)
        29
      else
        COMMON_YEAR_DAYS_IN_MONTH[month]
      end
    end

    # Returns <tt>Time.zone.now</tt> when <tt>Time.zone</tt> or <tt>config.time_zone</tt> are set, otherwise just returns <tt>Time.now</tt>.
    def current
      ::Time.zone ? ::Time.zone.now : ::Time.now
    end

    # Layers additional behavior on Time.at so that ActiveSupport::TimeWithZone and DateTime
    # instances can be used when called with a single argument
    def at_with_coercion(*args)
      return at_without_coercion(*args) if args.size != 1

      # Time.at can be called with a time or numerical value
      time_or_number = args.first

      if time_or_number.is_a?(ActiveSupport::TimeWithZone) || time_or_number.is_a?(DateTime)
        at_without_coercion(time_or_number.to_f).getlocal
      else
        at_without_coercion(time_or_number)
      end
    end
    alias_method :at_without_coercion, :at
    alias_method :at, :at_with_coercion
  end

  # Returns the number of seconds since 00:00:00.
  #
  #   Time.new(2012, 8, 29,  0,  0,  0).seconds_since_midnight # => 0
  #   Time.new(2012, 8, 29, 12, 34, 56).seconds_since_midnight # => 45296
  #   Time.new(2012, 8, 29, 23, 59, 59).seconds_since_midnight # => 86399
  def seconds_since_midnight
    to_i - change(:hour => 0).to_i + (usec / 1.0e+6)
  end

  # Returns the number of seconds until 23:59:59.
  #
  #   Time.new(2012, 8, 29,  0,  0,  0).seconds_until_end_of_day # => 86399
  #   Time.new(2012, 8, 29, 12, 34, 56).seconds_until_end_of_day # => 41103
  #   Time.new(2012, 8, 29, 23, 59, 59).seconds_until_end_of_day # => 0
  def seconds_until_end_of_day
    end_of_day.to_i - to_i
  end

  # Returns a new Time where one or more of the elements have been changed according
  # to the +options+ parameter. The time options (<tt>:hour</tt>, <tt>:min</tt>,
  # <tt>:sec</tt>, <tt>:usec</tt>, <tt>:nsec</tt>) reset cascadingly, so if only
  # the hour is passed, then minute, sec, usec and nsec is set to 0. If the hour
  # and minute is passed, then sec, usec and nsec is set to 0. The +options+
  # parameter takes a hash with any of these keys: <tt>:year</tt>, <tt>:month</tt>,
  # <tt>:day</tt>, <tt>:hour</tt>, <tt>:min</tt>, <tt>:sec</tt>, <tt>:usec</tt>
  # <tt>:nsec</tt>. Pass either <tt>:usec</tt> or <tt>:nsec</tt>, not both.
  #
  #   Time.new(2012, 8, 29, 22, 35, 0).change(day: 1)              # => Time.new(2012, 8, 1, 22, 35, 0)
  #   Time.new(2012, 8, 29, 22, 35, 0).change(year: 1981, day: 1)  # => Time.new(1981, 8, 1, 22, 35, 0)
  #   Time.new(2012, 8, 29, 22, 35, 0).change(year: 1981, hour: 0) # => Time.new(1981, 8, 29, 0, 0, 0)
  def change(options)
    new_year  = options.fetch(:year, year)
    new_month = options.fetch(:month, month)
    new_day   = options.fetch(:day, day)
    new_hour  = options.fetch(:hour, hour)
    new_min   = options.fetch(:min, options[:hour] ? 0 : min)
    new_sec   = options.fetch(:sec, (options[:hour] || options[:min]) ? 0 : sec)

    if new_nsec = options[:nsec]
      raise ArgumentError, "Can't change both :nsec and :usec at the same time: #{options.inspect}" if options[:usec]
      new_usec = Rational(new_nsec, 1000)
    else
      new_usec  = options.fetch(:usec, (options[:hour] || options[:min] || options[:sec]) ? 0 : Rational(nsec, 1000))
    end

    if utc?
      ::Time.utc(new_year, new_month, new_day, new_hour, new_min, new_sec, new_usec)
    elsif zone
      ::Time.local(new_year, new_month, new_day, new_hour, new_min, new_sec, new_usec)
    else
      raise ArgumentError, 'argument out of range' if new_usec > 999999
      ::Time.new(new_year, new_month, new_day, new_hour, new_min, new_sec + (new_usec.to_r / 1000000), utc_offset)
    end
  end

  # Uses Date to provide precise Time calculations for years, months, and days
  # according to the proleptic Gregorian calendar. The +options+ parameter
  # takes a hash with any of these keys: <tt>:years</tt>, <tt>:months</tt>,
  # <tt>:weeks</tt>, <tt>:days</tt>, <tt>:hours</tt>, <tt>:minutes</tt>,
  # <tt>:seconds</tt>.
  def advance(options)
    unless options[:weeks].nil?
      options[:weeks], partial_weeks = options[:weeks].divmod(1)
      options[:days] = options.fetch(:days, 0) + 7 * partial_weeks
    end

    unless options[:days].nil?
      options[:days], partial_days = options[:days].divmod(1)
      options[:hours] = options.fetch(:hours, 0) + 24 * partial_days
    end

    d = to_date.advance(options)
    d = d.gregorian if d.julian?
    time_advanced_by_date = change(:year => d.year, :month => d.month, :day => d.day)
    seconds_to_advance = \
      options.fetch(:seconds, 0) +
      options.fetch(:minutes, 0) * 60 +
      options.fetch(:hours, 0) * 3600

    if seconds_to_advance.zero?
      time_advanced_by_date
    else
      time_advanced_by_date.since(seconds_to_advance)
    end
  end

  # Returns a new Time representing the time a number of seconds ago, this is basically a wrapper around the Numeric extension
  def ago(seconds)
    since(-seconds)
  end

  # Returns a new Time representing the time a number of seconds since the instance time
  def since(seconds)
    self + seconds
  rescue
    to_datetime.since(seconds)
  end
  alias :in :since

  # Returns a new Time representing the start of the day (0:00)
  def beginning_of_day
    #(self - seconds_since_midnight).change(usec: 0)
    change(:hour => 0)
  end
  alias :midnight :beginning_of_day
  alias :at_midnight :beginning_of_day
  alias :at_beginning_of_day :beginning_of_day

  # Returns a new Time representing the middle of the day (12:00)
  def middle_of_day
    change(:hour => 12)
  end
  alias :midday :middle_of_day
  alias :noon :middle_of_day
  alias :at_midday :middle_of_day
  alias :at_noon :middle_of_day
  alias :at_middle_of_day :middle_of_day

  # Returns a new Time representing the end of the day, 23:59:59.999999
  def end_of_day
    change(
      :hour => 23,
      :min => 59,
      :sec => 59,
      :usec => Rational(999999999, 1000)
    )
  end
  alias :at_end_of_day :end_of_day

  # Returns a new Time representing the start of the hour (x:00)
  def beginning_of_hour
    change(:min => 0)
  end
  alias :at_beginning_of_hour :beginning_of_hour

  # Returns a new Time representing the end of the hour, x:59:59.999999
  def end_of_hour
    change(
      :min => 59,
      :sec => 59,
      :usec => Rational(999999999, 1000)
    )
  end
  alias :at_end_of_hour :end_of_hour

  # Returns a new Time representing the start of the minute (x:xx:00)
  def beginning_of_minute
    change(:sec => 0)
  end
  alias :at_beginning_of_minute :beginning_of_minute

  # Returns a new Time representing the end of the minute, x:xx:59.999999
  def end_of_minute
    change(
      :sec => 59,
      :usec => Rational(999999999, 1000)
    )
  end
  alias :at_end_of_minute :end_of_minute

  # Returns a Range representing the whole day of the current time.
  def all_day
    beginning_of_day..end_of_day
  end

  def plus_with_duration(other) #:nodoc:
    if ActiveSupport::Duration === other
      other.since(self)
    else
      plus_without_duration(other)
    end
  end
  alias_method :plus_without_duration, :+
  alias_method :+, :plus_with_duration

  def minus_with_duration(other) #:nodoc:
    if ActiveSupport::Duration === other
      other.until(self)
    else
      minus_without_duration(other)
    end
  end
  alias_method :minus_without_duration, :-
  alias_method :-, :minus_with_duration

  # Time#- can also be used to determine the number of seconds between two Time instances.
  # We're layering on additional behavior so that ActiveSupport::TimeWithZone instances
  # are coerced into values that Time#- will recognize
  def minus_with_coercion(other)
    other = other.comparable_time if other.respond_to?(:comparable_time)
    other.is_a?(DateTime) ? to_f - other.to_f : minus_without_coercion(other)
  end
  alias_method :minus_without_coercion, :-
  alias_method :-, :minus_with_coercion

  # Layers additional behavior on Time#<=> so that DateTime and ActiveSupport::TimeWithZone instances
  # can be chronologically compared with a Time
  def compare_with_coercion(other)
    # we're avoiding Time#to_datetime and Time#to_time because they're expensive
    if other.class == Time
      compare_without_coercion(other)
    elsif other.is_a?(Time)
      compare_without_coercion(other.to_time)
    else
      to_datetime <=> other
    end
  end
  alias_method :compare_without_coercion, :<=>
  alias_method :<=>, :compare_with_coercion

  # Layers additional behavior on Time#eql? so that ActiveSupport::TimeWithZone instances
  # can be eql? to an equivalent Time
  def eql_with_coercion(other)
    # if other is an ActiveSupport::TimeWithZone, coerce a Time instance from it so we can do eql? comparison
    other = other.comparable_time if other.respond_to?(:comparable_time)
    eql_without_coercion(other)
  end
  alias_method :eql_without_coercion, :eql?
  alias_method :eql?, :eql_with_coercion

end
require 'active_support/inflector/methods'
require 'active_support/values/time_zone'

class Time
  DATE_FORMATS = {
    :db           => '%Y-%m-%d %H:%M:%S',
    :number       => '%Y%m%d%H%M%S',
    :nsec         => '%Y%m%d%H%M%S%9N',
    :time         => '%H:%M',
    :short        => '%d %b %H:%M',
    :long         => '%B %d, %Y %H:%M',
    :long_ordinal => lambda { |time|
      day_format = ActiveSupport::Inflector.ordinalize(time.day)
      time.strftime("%B #{day_format}, %Y %H:%M")
    },
    :rfc822       => lambda { |time|
      offset_format = time.formatted_offset(false)
      time.strftime("%a, %d %b %Y %H:%M:%S #{offset_format}")
    },
    :iso8601      => lambda { |time| time.iso8601 }
  }

  # Converts to a formatted string. See DATE_FORMATS for built-in formats.
  #
  # This method is aliased to <tt>to_s</tt>.
  #
  #   time = Time.now                    # => Thu Jan 18 06:10:17 CST 2007
  #
  #   time.to_formatted_s(:time)         # => "06:10"
  #   time.to_s(:time)                   # => "06:10"
  #
  #   time.to_formatted_s(:db)           # => "2007-01-18 06:10:17"
  #   time.to_formatted_s(:number)       # => "20070118061017"
  #   time.to_formatted_s(:short)        # => "18 Jan 06:10"
  #   time.to_formatted_s(:long)         # => "January 18, 2007 06:10"
  #   time.to_formatted_s(:long_ordinal) # => "January 18th, 2007 06:10"
  #   time.to_formatted_s(:rfc822)       # => "Thu, 18 Jan 2007 06:10:17 -0600"
  #   time.to_formatted_s(:iso8601)      # => "2007-01-18T06:10:17-06:00"
  #
  # == Adding your own time formats to +to_formatted_s+
  # You can add your own formats to the Time::DATE_FORMATS hash.
  # Use the format name as the hash key and either a strftime string
  # or Proc instance that takes a time argument as the value.
  #
  #   # config/initializers/time_formats.rb
  #   Time::DATE_FORMATS[:month_and_year] = '%B %Y'
  #   Time::DATE_FORMATS[:short_ordinal]  = ->(time) { time.strftime("%B #{time.day.ordinalize}") }
  def to_formatted_s(format = :default)
    if formatter = DATE_FORMATS[format]
      formatter.respond_to?(:call) ? formatter.call(self).to_s : strftime(formatter)
    else
      to_default_s
    end
  end
  alias_method :to_default_s, :to_s
  alias_method :to_s, :to_formatted_s

  # Returns the UTC offset as an +HH:MM formatted string.
  #
  #   Time.local(2000).formatted_offset        # => "-06:00"
  #   Time.local(2000).formatted_offset(false) # => "-0600"
  def formatted_offset(colon = true, alternate_utc_string = nil)
    utc? && alternate_utc_string || ActiveSupport::TimeZone.seconds_to_utc_offset(utc_offset, colon)
  end
end
require 'active_support/deprecation'

ActiveSupport::Deprecation.warn("This is deprecated and will be removed in Rails 5.1 with no replacement.")
require 'active_support/time_with_zone'
require 'active_support/core_ext/time/acts_like'
require 'active_support/core_ext/date_and_time/zones'

class Time
  include DateAndTime::Zones
  class << self
    attr_accessor :zone_default

    # Returns the TimeZone for the current request, if this has been set (via Time.zone=).
    # If <tt>Time.zone</tt> has not been set for the current request, returns the TimeZone specified in <tt>config.time_zone</tt>.
    def zone
      Thread.current[:time_zone] || zone_default
    end

    # Sets <tt>Time.zone</tt> to a TimeZone object for the current request/thread.
    #
    # This method accepts any of the following:
    #
    # * A Rails TimeZone object.
    # * An identifier for a Rails TimeZone object (e.g., "Eastern Time (US & Canada)", <tt>-5.hours</tt>).
    # * A TZInfo::Timezone object.
    # * An identifier for a TZInfo::Timezone object (e.g., "America/New_York").
    #
    # Here's an example of how you might set <tt>Time.zone</tt> on a per request basis and reset it when the request is done.
    # <tt>current_user.time_zone</tt> just needs to return a string identifying the user's preferred time zone:
    #
    #   class ApplicationController < ActionController::Base
    #     around_action :set_time_zone
    #
    #     def set_time_zone
    #       if logged_in?
    #         Time.use_zone(current_user.time_zone) { yield }
    #       else
    #         yield
    #       end
    #     end
    #   end
    def zone=(time_zone)
      Thread.current[:time_zone] = find_zone!(time_zone)
    end

    # Allows override of <tt>Time.zone</tt> locally inside supplied block; resets <tt>Time.zone</tt> to existing value when done.
    def use_zone(time_zone)
      new_zone = find_zone!(time_zone)
      begin
        old_zone, ::Time.zone = ::Time.zone, new_zone
        yield
      ensure
        ::Time.zone = old_zone
      end
    end

    # Returns a TimeZone instance matching the time zone provided.
    # Accepts the time zone in any format supported by <tt>Time.zone=</tt>.
    # Raises an ArgumentError for invalid time zones.
    #
    #   Time.find_zone! "America/New_York" # => #<ActiveSupport::TimeZone @name="America/New_York" ...>
    #   Time.find_zone! "EST"              # => #<ActiveSupport::TimeZone @name="EST" ...>
    #   Time.find_zone! -5.hours           # => #<ActiveSupport::TimeZone @name="Bogota" ...>
    #   Time.find_zone! nil                # => nil
    #   Time.find_zone! false              # => false
    #   Time.find_zone! "NOT-A-TIMEZONE"   # => ArgumentError: Invalid Timezone: NOT-A-TIMEZONE
    def find_zone!(time_zone)
      if !time_zone || time_zone.is_a?(ActiveSupport::TimeZone)
        time_zone
      else
        # lookup timezone based on identifier (unless we've been passed a TZInfo::Timezone)
        unless time_zone.respond_to?(:period_for_local)
          time_zone = ActiveSupport::TimeZone[time_zone] || TZInfo::Timezone.get(time_zone)
        end

        # Return if a TimeZone instance, or wrap in a TimeZone instance if a TZInfo::Timezone
        if time_zone.is_a?(ActiveSupport::TimeZone)
          time_zone
        else
          ActiveSupport::TimeZone.create(time_zone.name, nil, time_zone)
        end
      end
    rescue TZInfo::InvalidTimezoneIdentifier
      raise ArgumentError, "Invalid Timezone: #{time_zone}"
    end

    # Returns a TimeZone instance matching the time zone provided.
    # Accepts the time zone in any format supported by <tt>Time.zone=</tt>.
    # Returns +nil+ for invalid time zones.
    #
    #   Time.find_zone "America/New_York" # => #<ActiveSupport::TimeZone @name="America/New_York" ...>
    #   Time.find_zone "NOT-A-TIMEZONE"   # => nil
    def find_zone(time_zone)
      find_zone!(time_zone) rescue nil
    end
  end
end
# encoding: utf-8

require 'uri'
str = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E" # Ni-ho-nn-go in UTF-8, means Japanese.
parser = URI::Parser.new

unless str == parser.unescape(parser.escape(str))
  URI::Parser.class_eval do
    remove_method :unescape
    def unescape(str, escaped = /%[a-fA-F\d]{2}/)
      # TODO: Are we actually sure that ASCII == UTF-8?
      # YK: My initial experiments say yes, but let's be sure please
      enc = str.encoding
      enc = Encoding::UTF_8 if enc == Encoding::US_ASCII
      str.gsub(escaped) { [$&[1, 2].hex].pack('C') }.force_encoding(enc)
    end
  end
end

module URI
  class << self
    def parser
      @parser ||= URI::Parser.new
    end
  end
end
require 'set'
require 'thread'
require 'thread_safe'
require 'pathname'
require 'active_support/core_ext/module/aliasing'
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/core_ext/module/introspection'
require 'active_support/core_ext/module/anonymous'
require 'active_support/core_ext/module/qualified_const'
require 'active_support/core_ext/object/blank'
require 'active_support/core_ext/kernel/reporting'
require 'active_support/core_ext/load_error'
require 'active_support/core_ext/name_error'
require 'active_support/core_ext/string/starts_ends_with'
require 'active_support/inflector'

module ActiveSupport #:nodoc:
  module Dependencies #:nodoc:
    extend self

    # Should we turn on Ruby warnings on the first load of dependent files?
    mattr_accessor :warnings_on_first_load
    self.warnings_on_first_load = false

    # All files ever loaded.
    mattr_accessor :history
    self.history = Set.new

    # All files currently loaded.
    mattr_accessor :loaded
    self.loaded = Set.new

    # Stack of files being loaded.
    mattr_accessor :loading
    self.loading = []

    # Should we load files or require them?
    mattr_accessor :mechanism
    self.mechanism = ENV['NO_RELOAD'] ? :require : :load

    # The set of directories from which we may automatically load files. Files
    # under these directories will be reloaded on each request in development mode,
    # unless the directory also appears in autoload_once_paths.
    mattr_accessor :autoload_paths
    self.autoload_paths = []

    # The set of directories from which automatically loaded constants are loaded
    # only once. All directories in this set must also be present in +autoload_paths+.
    mattr_accessor :autoload_once_paths
    self.autoload_once_paths = []

    # An array of qualified constant names that have been loaded. Adding a name
    # to this array will cause it to be unloaded the next time Dependencies are
    # cleared.
    mattr_accessor :autoloaded_constants
    self.autoloaded_constants = []

    # An array of constant names that need to be unloaded on every request. Used
    # to allow arbitrary constants to be marked for unloading.
    mattr_accessor :explicitly_unloadable_constants
    self.explicitly_unloadable_constants = []

    # The logger is used for generating information on the action run-time
    # (including benchmarking) if available. Can be set to nil for no logging.
    # Compatible with both Ruby's own Logger and Log4r loggers.
    mattr_accessor :logger

    # Set to +true+ to enable logging of const_missing and file loads.
    mattr_accessor :log_activity
    self.log_activity = false

    # The WatchStack keeps a stack of the modules being watched as files are
    # loaded. If a file in the process of being loaded (parent.rb) triggers the
    # load of another file (child.rb) the stack will ensure that child.rb
    # handles the new constants.
    #
    # If child.rb is being autoloaded, its constants will be added to
    # autoloaded_constants. If it was being `require`d, they will be discarded.
    #
    # This is handled by walking back up the watch stack and adding the constants
    # found by child.rb to the list of original constants in parent.rb.
    class WatchStack
      include Enumerable

      # @watching is a stack of lists of constants being watched. For instance,
      # if parent.rb is autoloaded, the stack will look like [[Object]]. If
      # parent.rb then requires namespace/child.rb, the stack will look like
      # [[Object], [Namespace]].

      def initialize
        @watching = []
        @stack = Hash.new { |h,k| h[k] = [] }
      end

      def each(&block)
        @stack.each(&block)
      end

      def watching?
        !@watching.empty?
      end

      # Returns a list of new constants found since the last call to
      # <tt>watch_namespaces</tt>.
      def new_constants
        constants = []

        # Grab the list of namespaces that we're looking for new constants under
        @watching.last.each do |namespace|
          # Retrieve the constants that were present under the namespace when watch_namespaces
          # was originally called
          original_constants = @stack[namespace].last

          mod = Inflector.constantize(namespace) if Dependencies.qualified_const_defined?(namespace)
          next unless mod.is_a?(Module)

          # Get a list of the constants that were added
          new_constants = mod.local_constants - original_constants

          # self[namespace] returns an Array of the constants that are being evaluated
          # for that namespace. For instance, if parent.rb requires child.rb, the first
          # element of self[Object] will be an Array of the constants that were present
          # before parent.rb was required. The second element will be an Array of the
          # constants that were present before child.rb was required.
          @stack[namespace].each do |namespace_constants|
            namespace_constants.concat(new_constants)
          end

          # Normalize the list of new constants, and add them to the list we will return
          new_constants.each do |suffix|
            constants << ([namespace, suffix] - ["Object"]).join("::")
          end
        end
        constants
      ensure
        # A call to new_constants is always called after a call to watch_namespaces
        pop_modules(@watching.pop)
      end

      # Add a set of modules to the watch stack, remembering the initial
      # constants.
      def watch_namespaces(namespaces)
        @watching << namespaces.map do |namespace|
          module_name = Dependencies.to_constant_name(namespace)
          original_constants = Dependencies.qualified_const_defined?(module_name) ?
            Inflector.constantize(module_name).local_constants : []

          @stack[module_name] << original_constants
          module_name
        end
      end

      private
      def pop_modules(modules)
        modules.each { |mod| @stack[mod].pop }
      end
    end

    # An internal stack used to record which constants are loaded by any block.
    mattr_accessor :constant_watch_stack
    self.constant_watch_stack = WatchStack.new

    # Module includes this module.
    module ModuleConstMissing #:nodoc:
      def self.append_features(base)
        base.class_eval do
          # Emulate #exclude via an ivar
          return if defined?(@_const_missing) && @_const_missing
          @_const_missing = instance_method(:const_missing)
          remove_method(:const_missing)
        end
        super
      end

      def self.exclude_from(base)
        base.class_eval do
          define_method :const_missing, @_const_missing
          @_const_missing = nil
        end
      end

      def const_missing(const_name)
        from_mod = anonymous? ? guess_for_anonymous(const_name) : self
        Dependencies.load_missing_constant(from_mod, const_name)
      end

      # We assume that the name of the module reflects the nesting
      # (unless it can be proven that is not the case) and the path to the file
      # that defines the constant. Anonymous modules cannot follow these
      # conventions and therefore we assume that the user wants to refer to a
      # top-level constant.
      def guess_for_anonymous(const_name)
        if Object.const_defined?(const_name)
          raise NameError.new "#{const_name} cannot be autoloaded from an anonymous class or module", const_name
        else
          Object
        end
      end

      def unloadable(const_desc = self)
        super(const_desc)
      end
    end

    # Object includes this module.
    module Loadable #:nodoc:
      def self.exclude_from(base)
        base.class_eval do
          define_method(:load, Kernel.instance_method(:load))
          private :load
        end
      end

      def require_or_load(file_name)
        Dependencies.require_or_load(file_name)
      end

      # Interprets a file using <tt>mechanism</tt> and marks its defined
      # constants as autoloaded. <tt>file_name</tt> can be either a string or
      # respond to <tt>to_path</tt>.
      #
      # Use this method in code that absolutely needs a certain constant to be
      # defined at that point. A typical use case is to make constant name
      # resolution deterministic for constants with the same relative name in
      # different namespaces whose evaluation would depend on load order
      # otherwise.
      def require_dependency(file_name, message = "No such file to load -- %s")
        file_name = file_name.to_path if file_name.respond_to?(:to_path)
        unless file_name.is_a?(String)
          raise ArgumentError, "the file name must either be a String or implement #to_path -- you passed #{file_name.inspect}"
        end

        Dependencies.depend_on(file_name, message)
      end

      def load_dependency(file)
        if Dependencies.load? && ActiveSupport::Dependencies.constant_watch_stack.watching?
          Dependencies.new_constants_in(Object) { yield }
        else
          yield
        end
      rescue Exception => exception  # errors from loading file
        exception.blame_file! file if exception.respond_to? :blame_file!
        raise
      end

      # Mark the given constant as unloadable. Unloadable constants are removed
      # each time dependencies are cleared.
      #
      # Note that marking a constant for unloading need only be done once. Setup
      # or init scripts may list each unloadable constant that may need unloading;
      # each constant will be removed for every subsequent clear, as opposed to
      # for the first clear.
      #
      # The provided constant descriptor may be a (non-anonymous) module or class,
      # or a qualified constant name as a string or symbol.
      #
      # Returns +true+ if the constant was not previously marked for unloading,
      # +false+ otherwise.
      def unloadable(const_desc)
        Dependencies.mark_for_unload const_desc
      end

      private

      def load(file, wrap = false)
        result = false
        load_dependency(file) { result = super }
        result
      end

      def require(file)
        result = false
        load_dependency(file) { result = super }
        result
      end
    end

    # Exception file-blaming.
    module Blamable #:nodoc:
      def blame_file!(file)
        (@blamed_files ||= []).unshift file
      end

      def blamed_files
        @blamed_files ||= []
      end

      def describe_blame
        return nil if blamed_files.empty?
        "This error occurred while loading the following files:\n   #{blamed_files.join "\n   "}"
      end

      def copy_blame!(exc)
        @blamed_files = exc.blamed_files.clone
        self
      end
    end

    def hook!
      Object.class_eval { include Loadable }
      Module.class_eval { include ModuleConstMissing }
      Exception.class_eval { include Blamable }
    end

    def unhook!
      ModuleConstMissing.exclude_from(Module)
      Loadable.exclude_from(Object)
    end

    def load?
      mechanism == :load
    end

    def depend_on(file_name, message = "No such file to load -- %s.rb")
      path = search_for_file(file_name)
      require_or_load(path || file_name)
    rescue LoadError => load_error
      if file_name = load_error.message[/ -- (.*?)(\.rb)?$/, 1]
        load_error.message.replace(message % file_name)
        load_error.copy_blame!(load_error)
      end
      raise
    end

    def clear
      log_call
      loaded.clear
      loading.clear
      remove_unloadable_constants!
    end

    def require_or_load(file_name, const_path = nil)
      log_call file_name, const_path
      file_name = $` if file_name =~ /\.rb\z/
      expanded = File.expand_path(file_name)
      return if loaded.include?(expanded)

      # Record that we've seen this file *before* loading it to avoid an
      # infinite loop with mutual dependencies.
      loaded << expanded
      loading << expanded

      begin
        if load?
          log "loading #{file_name}"

          # Enable warnings if this file has not been loaded before and
          # warnings_on_first_load is set.
          load_args = ["#{file_name}.rb"]
          load_args << const_path unless const_path.nil?

          if !warnings_on_first_load or history.include?(expanded)
            result = load_file(*load_args)
          else
            enable_warnings { result = load_file(*load_args) }
          end
        else
          log "requiring #{file_name}"
          result = require file_name
        end
      rescue Exception
        loaded.delete expanded
        raise
      ensure
        loading.pop
      end

      # Record history *after* loading so first load gets warnings.
      history << expanded
      result
    end

    # Is the provided constant path defined?
    def qualified_const_defined?(path)
      Object.const_defined?(path, false)
    end

    # Given +path+, a filesystem path to a ruby file, return an array of
    # constant paths which would cause Dependencies to attempt to load this
    # file.
    def loadable_constants_for_path(path, bases = autoload_paths)
      path = $` if path =~ /\.rb\z/
      expanded_path = File.expand_path(path)
      paths = []

      bases.each do |root|
        expanded_root = File.expand_path(root)
        next unless %r{\A#{Regexp.escape(expanded_root)}(/|\\)} =~ expanded_path

        nesting = expanded_path[(expanded_root.size)..-1]
        nesting = nesting[1..-1] if nesting && nesting[0] == ?/
        next if nesting.blank?

        paths << nesting.camelize
      end

      paths.uniq!
      paths
    end

    # Search for a file in autoload_paths matching the provided suffix.
    def search_for_file(path_suffix)
      path_suffix = path_suffix.sub(/(\.rb)?$/, ".rb")

      autoload_paths.each do |root|
        path = File.join(root, path_suffix)
        return path if File.file? path
      end
      nil # Gee, I sure wish we had first_match ;-)
    end

    # Does the provided path_suffix correspond to an autoloadable module?
    # Instead of returning a boolean, the autoload base for this module is
    # returned.
    def autoloadable_module?(path_suffix)
      autoload_paths.each do |load_path|
        return load_path if File.directory? File.join(load_path, path_suffix)
      end
      nil
    end

    def load_once_path?(path)
      # to_s works around a ruby issue where String#starts_with?(Pathname)
      # will raise a TypeError: no implicit conversion of Pathname into String
      autoload_once_paths.any? { |base| path.starts_with? base.to_s }
    end

    # Attempt to autoload the provided module name by searching for a directory
    # matching the expected path suffix. If found, the module is created and
    # assigned to +into+'s constants with the name +const_name+. Provided that
    # the directory was loaded from a reloadable base path, it is added to the
    # set of constants that are to be unloaded.
    def autoload_module!(into, const_name, qualified_name, path_suffix)
      return nil unless base_path = autoloadable_module?(path_suffix)
      mod = Module.new
      into.const_set const_name, mod
      autoloaded_constants << qualified_name unless autoload_once_paths.include?(base_path)
      mod
    end

    # Load the file at the provided path. +const_paths+ is a set of qualified
    # constant names. When loading the file, Dependencies will watch for the
    # addition of these constants. Each that is defined will be marked as
    # autoloaded, and will be removed when Dependencies.clear is next called.
    #
    # If the second parameter is left off, then Dependencies will construct a
    # set of names that the file at +path+ may define. See
    # +loadable_constants_for_path+ for more details.
    def load_file(path, const_paths = loadable_constants_for_path(path))
      log_call path, const_paths
      const_paths = [const_paths].compact unless const_paths.is_a? Array
      parent_paths = const_paths.collect { |const_path| const_path[/.*(?=::)/] || ::Object }

      result = nil
      newly_defined_paths = new_constants_in(*parent_paths) do
        result = Kernel.load path
      end

      autoloaded_constants.concat newly_defined_paths unless load_once_path?(path)
      autoloaded_constants.uniq!
      log "loading #{path} defined #{newly_defined_paths * ', '}" unless newly_defined_paths.empty?
      result
    end

    # Returns the constant path for the provided parent and constant name.
    def qualified_name_for(mod, name)
      mod_name = to_constant_name mod
      mod_name == "Object" ? name.to_s : "#{mod_name}::#{name}"
    end

    # Load the constant named +const_name+ which is missing from +from_mod+. If
    # it is not possible to load the constant into from_mod, try its parent
    # module using +const_missing+.
    def load_missing_constant(from_mod, const_name)
      log_call from_mod, const_name

      unless qualified_const_defined?(from_mod.name) && Inflector.constantize(from_mod.name).equal?(from_mod)
        raise ArgumentError, "A copy of #{from_mod} has been removed from the module tree but is still active!"
      end

      qualified_name = qualified_name_for from_mod, const_name
      path_suffix = qualified_name.underscore

      file_path = search_for_file(path_suffix)

      if file_path
        expanded = File.expand_path(file_path)
        expanded.sub!(/\.rb\z/, '')

        if loading.include?(expanded)
          raise "Circular dependency detected while autoloading constant #{qualified_name}"
        else
          require_or_load(expanded, qualified_name)
          raise LoadError, "Unable to autoload constant #{qualified_name}, expected #{file_path} to define it" unless from_mod.const_defined?(const_name, false)
          return from_mod.const_get(const_name)
        end
      elsif mod = autoload_module!(from_mod, const_name, qualified_name, path_suffix)
        return mod
      elsif (parent = from_mod.parent) && parent != from_mod &&
            ! from_mod.parents.any? { |p| p.const_defined?(const_name, false) }
        # If our parents do not have a constant named +const_name+ then we are free
        # to attempt to load upwards. If they do have such a constant, then this
        # const_missing must be due to from_mod::const_name, which should not
        # return constants from from_mod's parents.
        begin
          # Since Ruby does not pass the nesting at the point the unknown
          # constant triggered the callback we cannot fully emulate constant
          # name lookup and need to make a trade-off: we are going to assume
          # that the nesting in the body of Foo::Bar is [Foo::Bar, Foo] even
          # though it might not be. Counterexamples are
          #
          #   class Foo::Bar
          #     Module.nesting # => [Foo::Bar]
          #   end
          #
          # or
          #
          #   module M::N
          #     module S::T
          #       Module.nesting # => [S::T, M::N]
          #     end
          #   end
          #
          # for example.
          return parent.const_missing(const_name)
        rescue NameError => e
          raise unless e.missing_name? qualified_name_for(parent, const_name)
        end
      end

      name_error = NameError.new("uninitialized constant #{qualified_name}", const_name)
      name_error.set_backtrace(caller.reject {|l| l.starts_with? __FILE__ })
      raise name_error
    end

    # Remove the constants that have been autoloaded, and those that have been
    # marked for unloading. Before each constant is removed a callback is sent
    # to its class/module if it implements +before_remove_const+.
    #
    # The callback implementation should be restricted to cleaning up caches, etc.
    # as the environment will be in an inconsistent state, e.g. other constants
    # may have already been unloaded and not accessible.
    def remove_unloadable_constants!
      autoloaded_constants.each { |const| remove_constant const }
      autoloaded_constants.clear
      Reference.clear!
      explicitly_unloadable_constants.each { |const| remove_constant const }
    end

    class ClassCache
      def initialize
        @store = ThreadSafe::Cache.new
      end

      def empty?
        @store.empty?
      end

      def key?(key)
        @store.key?(key)
      end

      def get(key)
        key = key.name if key.respond_to?(:name)
        @store[key] ||= Inflector.constantize(key)
      end
      alias :[] :get

      def safe_get(key)
        key = key.name if key.respond_to?(:name)
        @store[key] ||= Inflector.safe_constantize(key)
      end

      def store(klass)
        return self unless klass.respond_to?(:name)
        raise(ArgumentError, 'anonymous classes cannot be cached') if klass.name.empty?
        @store[klass.name] = klass
        self
      end

      def clear!
        @store.clear
      end
    end

    Reference = ClassCache.new

    # Store a reference to a class +klass+.
    def reference(klass)
      Reference.store klass
    end

    # Get the reference for class named +name+.
    # Raises an exception if referenced class does not exist.
    def constantize(name)
      Reference.get(name)
    end

    # Get the reference for class named +name+ if one exists.
    # Otherwise returns +nil+.
    def safe_constantize(name)
      Reference.safe_get(name)
    end

    # Determine if the given constant has been automatically loaded.
    def autoloaded?(desc)
      return false if desc.is_a?(Module) && desc.anonymous?
      name = to_constant_name desc
      return false unless qualified_const_defined?(name)
      return autoloaded_constants.include?(name)
    end

    # Will the provided constant descriptor be unloaded?
    def will_unload?(const_desc)
      autoloaded?(const_desc) ||
        explicitly_unloadable_constants.include?(to_constant_name(const_desc))
    end

    # Mark the provided constant name for unloading. This constant will be
    # unloaded on each request, not just the next one.
    def mark_for_unload(const_desc)
      name = to_constant_name const_desc
      if explicitly_unloadable_constants.include? name
        false
      else
        explicitly_unloadable_constants << name
        true
      end
    end

    # Run the provided block and detect the new constants that were loaded during
    # its execution. Constants may only be regarded as 'new' once -- so if the
    # block calls +new_constants_in+ again, then the constants defined within the
    # inner call will not be reported in this one.
    #
    # If the provided block does not run to completion, and instead raises an
    # exception, any new constants are regarded as being only partially defined
    # and will be removed immediately.
    def new_constants_in(*descs)
      log_call(*descs)

      constant_watch_stack.watch_namespaces(descs)
      aborting = true

      begin
        yield # Now yield to the code that is to define new constants.
        aborting = false
      ensure
        new_constants = constant_watch_stack.new_constants

        log "New constants: #{new_constants * ', '}"
        return new_constants unless aborting

        log "Error during loading, removing partially loaded constants "
        new_constants.each { |c| remove_constant(c) }.clear
      end

      []
    end

    # Convert the provided const desc to a qualified constant name (as a string).
    # A module, class, symbol, or string may be provided.
    def to_constant_name(desc) #:nodoc:
      case desc
        when String then desc.sub(/^::/, '')
        when Symbol then desc.to_s
        when Module
          desc.name ||
            raise(ArgumentError, "Anonymous modules have no name to be referenced by")
        else raise TypeError, "Not a valid constant descriptor: #{desc.inspect}"
      end
    end

    def remove_constant(const) #:nodoc:
      # Normalize ::Foo, ::Object::Foo, Object::Foo, Object::Object::Foo, etc. as Foo.
      normalized = const.to_s.sub(/\A::/, '')
      normalized.sub!(/\A(Object::)+/, '')

      constants = normalized.split('::')
      to_remove = constants.pop

      # Remove the file path from the loaded list.
      file_path = search_for_file(const.underscore)
      if file_path
        expanded = File.expand_path(file_path)
        expanded.sub!(/\.rb\z/, '')
        self.loaded.delete(expanded)
      end

      if constants.empty?
        parent = Object
      else
        # This method is robust to non-reachable constants.
        #
        # Non-reachable constants may be passed if some of the parents were
        # autoloaded and already removed. It is easier to do a sanity check
        # here than require the caller to be clever. We check the parent
        # rather than the very const argument because we do not want to
        # trigger Kernel#autoloads, see the comment below.
        parent_name = constants.join('::')
        return unless qualified_const_defined?(parent_name)
        parent = constantize(parent_name)
      end

      log "removing constant #{const}"

      # In an autoloaded user.rb like this
      #
      #   autoload :Foo, 'foo'
      #
      #   class User < ActiveRecord::Base
      #   end
      #
      # we correctly register "Foo" as being autoloaded. But if the app does
      # not use the "Foo" constant we need to be careful not to trigger
      # loading "foo.rb" ourselves. While #const_defined? and #const_get? do
      # require the file, #autoload? and #remove_const don't.
      #
      # We are going to remove the constant nonetheless ---which exists as
      # far as Ruby is concerned--- because if the user removes the macro
      # call from a class or module that were not autoloaded, as in the
      # example above with Object, accessing to that constant must err.
      unless parent.autoload?(to_remove)
        begin
          constantized = parent.const_get(to_remove, false)
        rescue NameError
          log "the constant #{const} is not reachable anymore, skipping"
          return
        else
          constantized.before_remove_const if constantized.respond_to?(:before_remove_const)
        end
      end

      begin
        parent.instance_eval { remove_const to_remove }
      rescue NameError
        log "the constant #{const} is not reachable anymore, skipping"
      end
    end

    protected
      def log_call(*args)
        if log_activity?
          arg_str = args.collect(&:inspect) * ', '
          /in `([a-z_\?\!]+)'/ =~ caller(1).first
          selector = $1 || '<unknown>'
          log "called #{selector}(#{arg_str})"
        end
      end

      def log(msg)
        logger.debug "Dependencies: #{msg}" if log_activity?
      end

      def log_activity?
        logger && log_activity
      end
  end
end

ActiveSupport::Dependencies.hook!
require "active_support/inflector/methods"

module ActiveSupport
  # Autoload and eager load conveniences for your library.
  #
  # This module allows you to define autoloads based on
  # Rails conventions (i.e. no need to define the path
  # it is automatically guessed based on the filename)
  # and also define a set of constants that needs to be
  # eager loaded:
  #
  #   module MyLib
  #     extend ActiveSupport::Autoload
  #
  #     autoload :Model
  #
  #     eager_autoload do
  #       autoload :Cache
  #     end
  #   end
  #
  # Then your library can be eager loaded by simply calling:
  #
  #   MyLib.eager_load!
  module Autoload
    def self.extended(base) # :nodoc:
      base.class_eval do
        @_autoloads = {}
        @_under_path = nil
        @_at_path = nil
        @_eager_autoload = false
      end
    end

    def autoload(const_name, path = @_at_path)
      unless path
        full = [name, @_under_path, const_name.to_s].compact.join("::")
        path = Inflector.underscore(full)
      end

      if @_eager_autoload
        @_autoloads[const_name] = path
      end

      super const_name, path
    end

    def autoload_under(path)
      @_under_path, old_path = path, @_under_path
      yield
    ensure
      @_under_path = old_path
    end

    def autoload_at(path)
      @_at_path, old_path = path, @_at_path
      yield
    ensure
      @_at_path = old_path
    end

    def eager_autoload
      old_eager, @_eager_autoload = @_eager_autoload, true
      yield
    ensure
      @_eager_autoload = old_eager
    end

    def eager_load!
      @_autoloads.each_value { |file| require file }
    end

    def autoloads
      @_autoloads
    end
  end
end
require 'singleton'

module ActiveSupport
  # \Deprecation specifies the API used by Rails to deprecate methods, instance
  # variables, objects and constants.
  class Deprecation
    # active_support.rb sets an autoload for ActiveSupport::Deprecation.
    #
    # If these requires were at the top of the file the constant would not be
    # defined by the time their files were loaded. Since some of them reopen
    # ActiveSupport::Deprecation its autoload would be triggered, resulting in
    # a circular require warning for active_support/deprecation.rb.
    #
    # So, we define the constant first, and load dependencies later.
    require 'active_support/deprecation/instance_delegator'
    require 'active_support/deprecation/behaviors'
    require 'active_support/deprecation/reporting'
    require 'active_support/deprecation/method_wrappers'
    require 'active_support/deprecation/proxy_wrappers'
    require 'active_support/core_ext/module/deprecation'

    include Singleton
    include InstanceDelegator
    include Behavior
    include Reporting
    include MethodWrapper

    # The version number in which the deprecated behavior will be removed, by default.
    attr_accessor :deprecation_horizon

    # It accepts two parameters on initialization. The first is a version of library
    # and the second is a library name
    #
    #   ActiveSupport::Deprecation.new('2.0', 'MyLibrary')
    def initialize(deprecation_horizon = '5.0', gem_name = 'Rails')
      self.gem_name = gem_name
      self.deprecation_horizon = deprecation_horizon
      # By default, warnings are not silenced and debugging is off.
      self.silenced = false
      self.debug = false
    end
  end
end
require "active_support/notifications"

module ActiveSupport
  class DeprecationException < StandardError
  end

  class Deprecation
    # Default warning behaviors per Rails.env.
    DEFAULT_BEHAVIORS = {
      raise: ->(message, callstack) {
        e = DeprecationException.new(message)
        e.set_backtrace(callstack)
        raise e
      },

      stderr: ->(message, callstack) {
        $stderr.puts(message)
        $stderr.puts callstack.join("\n  ") if debug
      },

      log: ->(message, callstack) {
        logger =
            if defined?(Rails.logger) && Rails.logger
              Rails.logger
            else
              require 'active_support/logger'
              ActiveSupport::Logger.new($stderr)
            end
        logger.warn message
        logger.debug callstack.join("\n  ") if debug
      },

      notify: ->(message, callstack) {
        ActiveSupport::Notifications.instrument("deprecation.rails",
                                                :message => message, :callstack => callstack)
      },

      silence: ->(message, callstack) {},
    }

    module Behavior
      # Whether to print a backtrace along with the warning.
      attr_accessor :debug

      # Returns the current behavior or if one isn't set, defaults to +:stderr+.
      def behavior
        @behavior ||= [DEFAULT_BEHAVIORS[:stderr]]
      end

      # Sets the behavior to the specified value. Can be a single value, array,
      # or an object that responds to +call+.
      #
      # Available behaviors:
      #
      # [+raise+]   Raise <tt>ActiveSupport::DeprecationException</tt>.
      # [+stderr+]  Log all deprecation warnings to +$stderr+.
      # [+log+]     Log all deprecation warnings to +Rails.logger+.
      # [+notify+]  Use +ActiveSupport::Notifications+ to notify +deprecation.rails+.
      # [+silence+] Do nothing.
      #
      # Setting behaviors only affects deprecations that happen after boot time.
      # Deprecation warnings raised by gems are not affected by this setting
      # because they happen before Rails boots up.
      #
      #   ActiveSupport::Deprecation.behavior = :stderr
      #   ActiveSupport::Deprecation.behavior = [:stderr, :log]
      #   ActiveSupport::Deprecation.behavior = MyCustomHandler
      #   ActiveSupport::Deprecation.behavior = ->(message, callstack) {
      #     # custom stuff
      #   }
      def behavior=(behavior)
        @behavior = Array(behavior).map { |b| DEFAULT_BEHAVIORS[b] || b }
      end
    end
  end
end
require 'active_support/core_ext/kernel/singleton_class'
require 'active_support/core_ext/module/delegation'

module ActiveSupport
  class Deprecation
    module InstanceDelegator # :nodoc:
      def self.included(base)
        base.extend(ClassMethods)
        base.public_class_method :new
      end

      module ClassMethods # :nodoc:
        def include(included_module)
          included_module.instance_methods.each { |m| method_added(m) }
          super
        end

        def method_added(method_name)
          singleton_class.delegate(method_name, to: :instance)
        end
      end
    end
  end
end
require 'active_support/core_ext/module/aliasing'
require 'active_support/core_ext/array/extract_options'

module ActiveSupport
  class Deprecation
    module MethodWrapper
      # Declare that a method has been deprecated.
      #
      #   module Fred
      #     extend self
      #
      #     def foo; end
      #     def bar; end
      #     def baz; end
      #   end
      #
      #   ActiveSupport::Deprecation.deprecate_methods(Fred, :foo, bar: :qux, baz: 'use Bar#baz instead')
      #   # => [:foo, :bar, :baz]
      #
      #   Fred.foo
      #   # => "DEPRECATION WARNING: foo is deprecated and will be removed from Rails 4.1."
      #
      #   Fred.bar
      #   # => "DEPRECATION WARNING: bar is deprecated and will be removed from Rails 4.1 (use qux instead)."
      #
      #   Fred.baz
      #   # => "DEPRECATION WARNING: baz is deprecated and will be removed from Rails 4.1 (use Bar#baz instead)."
      def deprecate_methods(target_module, *method_names)
        options = method_names.extract_options!
        deprecator = options.delete(:deprecator) || ActiveSupport::Deprecation.instance
        method_names += options.keys

        method_names.each do |method_name|
          mod = Module.new do
            define_method(method_name) do |*args, &block|
              deprecator.deprecation_warning(method_name, options[method_name])
              super(*args, &block)
            end
          end

          target_module.prepend(mod)
        end
      end
    end
  end
end
require 'active_support/inflector/methods'

module ActiveSupport
  class Deprecation
    class DeprecationProxy #:nodoc:
      def self.new(*args, &block)
        object = args.first

        return object unless object
        super
      end

      instance_methods.each { |m| undef_method m unless m =~ /^__|^object_id$/ }

      # Don't give a deprecation warning on inspect since test/unit and error
      # logs rely on it for diagnostics.
      def inspect
        target.inspect
      end

      private
        def method_missing(called, *args, &block)
          warn caller, called, args
          target.__send__(called, *args, &block)
        end
    end

    # DeprecatedObjectProxy transforms an object into a deprecated one. It
    # takes an object, a deprecation message and optionally a deprecator. The
    # deprecator defaults to +ActiveSupport::Deprecator+ if none is specified.
    #
    #   deprecated_object = ActiveSupport::Deprecation::DeprecatedObjectProxy.new(Object.new, "This object is now deprecated")
    #   # => #<Object:0x007fb9b34c34b0>
    #
    #   deprecated_object.to_s
    #   DEPRECATION WARNING: This object is now deprecated.
    #   (Backtrace)
    #   # => "#<Object:0x007fb9b34c34b0>"
    class DeprecatedObjectProxy < DeprecationProxy
      def initialize(object, message, deprecator = ActiveSupport::Deprecation.instance)
        @object = object
        @message = message
        @deprecator = deprecator
      end

      private
        def target
          @object
        end

        def warn(callstack, called, args)
          @deprecator.warn(@message, callstack)
        end
    end

    # DeprecatedInstanceVariableProxy transforms an instance variable into a
    # deprecated one. It takes an instance of a class, a method on that class
    # and an instance variable. It optionally takes a deprecator as the last
    # argument. The deprecator defaults to +ActiveSupport::Deprecator+ if none
    # is specified.
    #
    #   class Example
    #     def initialize
    #       @request = ActiveSupport::Deprecation::DeprecatedInstanceVariableProxy.new(self, :request, :@request)
    #       @_request = :special_request
    #     end
    #
    #     def request
    #       @_request
    #     end
    #
    #     def old_request
    #       @request
    #     end
    #   end
    #
    #   example = Example.new
    #   # => #<Example:0x007fb9b31090b8 @_request=:special_request, @request=:special_request>
    #
    #   example.old_request.to_s
    #   # => DEPRECATION WARNING: @request is deprecated! Call request.to_s instead of
    #      @request.to_s
    #      (Bactrace information…)
    #      "special_request"
    #
    #   example.request.to_s
    #   # => "special_request"
    class DeprecatedInstanceVariableProxy < DeprecationProxy
      def initialize(instance, method, var = "@#{method}", deprecator = ActiveSupport::Deprecation.instance)
        @instance = instance
        @method = method
        @var = var
        @deprecator = deprecator
      end

      private
        def target
          @instance.__send__(@method)
        end

        def warn(callstack, called, args)
          @deprecator.warn("#{@var} is deprecated! Call #{@method}.#{called} instead of #{@var}.#{called}. Args: #{args.inspect}", callstack)
        end
    end

    # DeprecatedConstantProxy transforms a constant into a deprecated one. It
    # takes the names of an old (deprecated) constant and of a new contstant
    # (both in string form) and optionally a deprecator. The deprecator defaults
    # to +ActiveSupport::Deprecator+ if none is specified. The deprecated constant
    # now returns the value of the new one.
    #
    #   PLANETS = %w(mercury venus earth mars jupiter saturn uranus neptune pluto)
    #
    #   (In a later update, the orignal implementation of `PLANETS` has been removed.)
    #
    #   PLANETS_POST_2006 = %w(mercury venus earth mars jupiter saturn uranus neptune)
    #   PLANETS = ActiveSupport::Deprecation::DeprecatedConstantProxy.new('PLANETS', 'PLANETS_POST_2006')
    #
    #   PLANETS.map { |planet| planet.capitalize }
    #   # => DEPRECATION WARNING: PLANETS is deprecated! Use PLANETS_POST_2006 instead.
    #        (Bactrace information…)
    #        ["Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"]
    class DeprecatedConstantProxy < DeprecationProxy
      def initialize(old_const, new_const, deprecator = ActiveSupport::Deprecation.instance)
        @old_const = old_const
        @new_const = new_const
        @deprecator = deprecator
      end

      def class
        target.class
      end

      private
        def target
          ActiveSupport::Inflector.constantize(@new_const.to_s)
        end

        def warn(callstack, called, args)
          @deprecator.warn("#{@old_const} is deprecated! Use #{@new_const} instead.", callstack)
        end
    end
  end
end
module ActiveSupport
  class Deprecation
    module Reporting
      # Whether to print a message (silent mode)
      attr_accessor :silenced
      # Name of gem where method is deprecated
      attr_accessor :gem_name

      # Outputs a deprecation warning to the output configured by
      # <tt>ActiveSupport::Deprecation.behavior</tt>.
      #
      #   ActiveSupport::Deprecation.warn('something broke!')
      #   # => "DEPRECATION WARNING: something broke! (called from your_code.rb:1)"
      def warn(message = nil, callstack = nil)
        return if silenced

        callstack ||= caller(2)
        deprecation_message(callstack, message).tap do |m|
          behavior.each { |b| b.call(m, callstack) }
        end
      end

      # Silence deprecation warnings within the block.
      #
      #   ActiveSupport::Deprecation.warn('something broke!')
      #   # => "DEPRECATION WARNING: something broke! (called from your_code.rb:1)"
      #
      #   ActiveSupport::Deprecation.silence do
      #     ActiveSupport::Deprecation.warn('something broke!')
      #   end
      #   # => nil
      def silence
        old_silenced, @silenced = @silenced, true
        yield
      ensure
        @silenced = old_silenced
      end

      def deprecation_warning(deprecated_method_name, message = nil, caller_backtrace = nil)
        caller_backtrace ||= caller(2)
        deprecated_method_warning(deprecated_method_name, message).tap do |msg|
          warn(msg, caller_backtrace)
        end
      end

      private
        # Outputs a deprecation warning message
        #
        #   ActiveSupport::Deprecation.deprecated_method_warning(:method_name)
        #   # => "method_name is deprecated and will be removed from Rails #{deprecation_horizon}"
        #   ActiveSupport::Deprecation.deprecated_method_warning(:method_name, :another_method)
        #   # => "method_name is deprecated and will be removed from Rails #{deprecation_horizon} (use another_method instead)"
        #   ActiveSupport::Deprecation.deprecated_method_warning(:method_name, "Optional message")
        #   # => "method_name is deprecated and will be removed from Rails #{deprecation_horizon} (Optional message)"
        def deprecated_method_warning(method_name, message = nil)
          warning = "#{method_name} is deprecated and will be removed from #{gem_name} #{deprecation_horizon}"
          case message
            when Symbol then "#{warning} (use #{message} instead)"
            when String then "#{warning} (#{message})"
            else warning
          end
        end

        def deprecation_message(callstack, message = nil)
          message ||= "You are using deprecated behavior which will be removed from the next major or minor release."
          message += '.' unless message =~ /\.$/
          "DEPRECATION WARNING: #{message} #{deprecation_caller_message(callstack)}"
        end

        def deprecation_caller_message(callstack)
          file, line, method = extract_callstack(callstack)
          if file
            if line && method
              "(called from #{method} at #{file}:#{line})"
            else
              "(called from #{file}:#{line})"
            end
          end
        end

        def extract_callstack(callstack)
          rails_gem_root = File.expand_path("../../../../..", __FILE__) + "/"
          offending_line = callstack.find { |line| !line.start_with?(rails_gem_root) } || callstack.first
          if offending_line
            if md = offending_line.match(/^(.+?):(\d+)(?::in `(.*?)')?/)
              md.captures
            else
              offending_line
            end
          end
        end
    end
  end
end
module ActiveSupport
  # This module provides an internal implementation to track descendants
  # which is faster than iterating through ObjectSpace.
  module DescendantsTracker
    @@direct_descendants = {}

    class << self
      def direct_descendants(klass)
        @@direct_descendants[klass] || []
      end

      def descendants(klass)
        arr = []
        accumulate_descendants(klass, arr)
        arr
      end

      def clear
        if defined? ActiveSupport::Dependencies
          @@direct_descendants.each do |klass, descendants|
            if ActiveSupport::Dependencies.autoloaded?(klass)
              @@direct_descendants.delete(klass)
            else
              descendants.reject! { |v| ActiveSupport::Dependencies.autoloaded?(v) }
            end
          end
        else
          @@direct_descendants.clear
        end
      end

      # This is the only method that is not thread safe, but is only ever called
      # during the eager loading phase.
      def store_inherited(klass, descendant)
        (@@direct_descendants[klass] ||= []) << descendant
      end

      private
      def accumulate_descendants(klass, acc)
        if direct_descendants = @@direct_descendants[klass]
          acc.concat(direct_descendants)
          direct_descendants.each { |direct_descendant| accumulate_descendants(direct_descendant, acc) }
        end
      end
    end

    def inherited(base)
      DescendantsTracker.store_inherited(self, base)
      super
    end

    def direct_descendants
      DescendantsTracker.direct_descendants(self)
    end

    def descendants
      DescendantsTracker.descendants(self)
    end
  end
end
require 'active_support/core_ext/array/conversions'
require 'active_support/core_ext/object/acts_like'

module ActiveSupport
  # Provides accurate date and time measurements using Date#advance and
  # Time#advance, respectively. It mainly supports the methods on Numeric.
  #
  #   1.month.ago       # equivalent to Time.now.advance(months: -1)
  class Duration
    attr_accessor :value, :parts

    def initialize(value, parts) #:nodoc:
      @value, @parts = value, parts
    end

    # Adds another Duration or a Numeric to this Duration. Numeric values
    # are treated as seconds.
    def +(other)
      if Duration === other
        Duration.new(value + other.value, @parts + other.parts)
      else
        Duration.new(value + other, @parts + [[:seconds, other]])
      end
    end

    # Subtracts another Duration or a Numeric from this Duration. Numeric
    # values are treated as seconds.
    def -(other)
      self + (-other)
    end

    def -@ #:nodoc:
      Duration.new(-value, parts.map { |type,number| [type, -number] })
    end

    def is_a?(klass) #:nodoc:
      Duration == klass || value.is_a?(klass)
    end
    alias :kind_of? :is_a?

    def instance_of?(klass) # :nodoc:
      Duration == klass || value.instance_of?(klass)
    end

    # Returns +true+ if +other+ is also a Duration instance with the
    # same +value+, or if <tt>other == value</tt>.
    def ==(other)
      if Duration === other
        other.value == value
      else
        other == value
      end
    end

    def to_s
      @value.to_s
    end

    # Returns the number of seconds that this Duration represents.
    #
    #   1.minute.to_i   # => 60
    #   1.hour.to_i     # => 3600
    #   1.day.to_i      # => 86400
    #
    # Note that this conversion makes some assumptions about the
    # duration of some periods, e.g. months are always 30 days
    # and years are 365.25 days:
    #
    #   # equivalent to 30.days.to_i
    #   1.month.to_i    # => 2592000
    #
    #   # equivalent to 365.25.days.to_i
    #   1.year.to_i     # => 31557600
    #
    # In such cases, Ruby's core
    # Date[http://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html] and
    # Time[http://ruby-doc.org/stdlib/libdoc/time/rdoc/Time.html] should be used for precision
    # date and time arithmetic.
    def to_i
      @value.to_i
    end

    # Returns +true+ if +other+ is also a Duration instance, which has the
    # same parts as this one.
    def eql?(other)
      Duration === other && other.value.eql?(value)
    end

    def hash
      @value.hash
    end

    def self.===(other) #:nodoc:
      other.is_a?(Duration)
    rescue ::NoMethodError
      false
    end

    # Calculates a new Time or Date that is as far in the future
    # as this Duration represents.
    def since(time = ::Time.current)
      sum(1, time)
    end
    alias :from_now :since

    # Calculates a new Time or Date that is as far in the past
    # as this Duration represents.
    def ago(time = ::Time.current)
      sum(-1, time)
    end
    alias :until :ago

    def inspect #:nodoc:
      parts.
        reduce(::Hash.new(0)) { |h,(l,r)| h[l] += r; h }.
        sort_by {|unit,  _ | [:years, :months, :days, :minutes, :seconds].index(unit)}.
        map     {|unit, val| "#{val} #{val == 1 ? unit.to_s.chop : unit.to_s}"}.
        to_sentence(locale: ::I18n.default_locale)
    end

    def as_json(options = nil) #:nodoc:
      to_i
    end

    def respond_to_missing?(method, include_private=false) #:nodoc:
      @value.respond_to?(method, include_private)
    end

    delegate :<=>, to: :value

    protected

      def sum(sign, time = ::Time.current) #:nodoc:
        parts.inject(time) do |t,(type,number)|
          if t.acts_like?(:time) || t.acts_like?(:date)
            if type == :seconds
              t.since(sign * number)
            else
              t.advance(type => sign * number)
            end
          else
            raise ::ArgumentError, "expected a time or date, got #{time.inspect}"
          end
        end
      end

    private

      def method_missing(method, *args, &block) #:nodoc:
        value.send(method, *args, &block)
      end
  end
end
module ActiveSupport
  # FileUpdateChecker specifies the API used by Rails to watch files
  # and control reloading. The API depends on four methods:
  #
  # * +initialize+ which expects two parameters and one block as
  #   described below.
  #
  # * +updated?+ which returns a boolean if there were updates in
  #   the filesystem or not.
  #
  # * +execute+ which executes the given block on initialization
  #   and updates the latest watched files and timestamp.
  #
  # * +execute_if_updated+ which just executes the block if it was updated.
  #
  # After initialization, a call to +execute_if_updated+ must execute
  # the block only if there was really a change in the filesystem.
  #
  # This class is used by Rails to reload the I18n framework whenever
  # they are changed upon a new request.
  #
  #   i18n_reloader = ActiveSupport::FileUpdateChecker.new(paths) do
  #     I18n.reload!
  #   end
  #
  #   ActionDispatch::Reloader.to_prepare do
  #     i18n_reloader.execute_if_updated
  #   end
  class FileUpdateChecker
    # It accepts two parameters on initialization. The first is an array
    # of files and the second is an optional hash of directories. The hash must
    # have directories as keys and the value is an array of extensions to be
    # watched under that directory.
    #
    # This method must also receive a block that will be called once a path
    # changes. The array of files and list of directories cannot be changed
    # after FileUpdateChecker has been initialized.
    def initialize(files, dirs={}, &block)
      @files = files.freeze
      @glob  = compile_glob(dirs)
      @block = block

      @watched    = nil
      @updated_at = nil

      @last_watched   = watched
      @last_update_at = updated_at(@last_watched)
    end

    # Check if any of the entries were updated. If so, the watched and/or
    # updated_at values are cached until the block is executed via +execute+
    # or +execute_if_updated+.
    def updated?
      current_watched = watched
      if @last_watched.size != current_watched.size
        @watched = current_watched
        true
      else
        current_updated_at = updated_at(current_watched)
        if @last_update_at < current_updated_at
          @watched    = current_watched
          @updated_at = current_updated_at
          true
        else
          false
        end
      end
    end

    # Executes the given block and updates the latest watched files and
    # timestamp.
    def execute
      @last_watched   = watched
      @last_update_at = updated_at(@last_watched)
      @block.call
    ensure
      @watched = nil
      @updated_at = nil
    end

    # Execute the block given if updated.
    def execute_if_updated
      if updated?
        execute
        true
      else
        false
      end
    end

    private

    def watched
      @watched || begin
        all = @files.select { |f| File.exist?(f) }
        all.concat(Dir[@glob]) if @glob
        all
      end
    end

    def updated_at(paths)
      @updated_at || max_mtime(paths) || Time.at(0)
    end

    # This method returns the maximum mtime of the files in +paths+, or +nil+
    # if the array is empty.
    #
    # Files with a mtime in the future are ignored. Such abnormal situation
    # can happen for example if the user changes the clock by hand. It is
    # healthy to consider this edge case because with mtimes in the future
    # reloading is not triggered.
    def max_mtime(paths)
      time_now = Time.now
      paths.map {|path| File.mtime(path)}.reject {|mtime| time_now < mtime}.max
    end

    def compile_glob(hash)
      hash.freeze # Freeze so changes aren't accidentally pushed
      return if hash.empty?

      globs = hash.map do |key, value|
        "#{escape(key)}/**/*#{compile_ext(value)}"
      end
      "{#{globs.join(",")}}"
    end

    def escape(key)
      key.gsub(',','\,')
    end

    def compile_ext(array)
      array = Array(array)
      return if array.empty?
      ".{#{array.join(",")}}"
    end
  end
end
module ActiveSupport
  # Returns the version of the currently loaded Active Support as a <tt>Gem::Version</tt>
  def self.gem_version
    Gem::Version.new VERSION::STRING
  end

  module VERSION
    MAJOR = 5
    MINOR = 0
    TINY  = 0
    PRE   = "alpha"

    STRING = [MAJOR, MINOR, TINY, PRE].compact.join(".")
  end
end
require 'zlib'
require 'stringio'

module ActiveSupport
  # A convenient wrapper for the zlib standard library that allows
  # compression/decompression of strings with gzip.
  #
  #   gzip = ActiveSupport::Gzip.compress('compress me!')
  #   # => "\x1F\x8B\b\x00o\x8D\xCDO\x00\x03K\xCE\xCF-(J-.V\xC8MU\x04\x00R>n\x83\f\x00\x00\x00"
  #
  #   ActiveSupport::Gzip.decompress(gzip)
  #   # => "compress me!" 
  module Gzip
    class Stream < StringIO
      def initialize(*)
        super
        set_encoding "BINARY"
      end
      def close; rewind; end
    end

    # Decompresses a gzipped string.
    def self.decompress(source)
      Zlib::GzipReader.new(StringIO.new(source)).read
    end

    # Compresses a string using gzip.
    def self.compress(source, level=Zlib::DEFAULT_COMPRESSION, strategy=Zlib::DEFAULT_STRATEGY)
      output = Stream.new
      gz = Zlib::GzipWriter.new(output, level, strategy)
      gz.write(source)
      gz.close
      output.string
    end
  end
end
require 'active_support/core_ext/hash/keys'
require 'active_support/core_ext/hash/reverse_merge'

module ActiveSupport
  # Implements a hash where keys <tt>:foo</tt> and <tt>"foo"</tt> are considered
  # to be the same.
  #
  #   rgb = ActiveSupport::HashWithIndifferentAccess.new
  #
  #   rgb[:black] = '#000000'
  #   rgb[:black]  # => '#000000'
  #   rgb['black'] # => '#000000'
  #
  #   rgb['white'] = '#FFFFFF'
  #   rgb[:white]  # => '#FFFFFF'
  #   rgb['white'] # => '#FFFFFF'
  #
  # Internally symbols are mapped to strings when used as keys in the entire
  # writing interface (calling <tt>[]=</tt>, <tt>merge</tt>, etc). This
  # mapping belongs to the public interface. For example, given:
  #
  #   hash = ActiveSupport::HashWithIndifferentAccess.new(a: 1)
  #
  # You are guaranteed that the key is returned as a string:
  #
  #   hash.keys # => ["a"]
  #
  # Technically other types of keys are accepted:
  #
  #   hash = ActiveSupport::HashWithIndifferentAccess.new(a: 1)
  #   hash[0] = 0
  #   hash # => {"a"=>1, 0=>0}
  #
  # but this class is intended for use cases where strings or symbols are the
  # expected keys and it is convenient to understand both as the same. For
  # example the +params+ hash in Ruby on Rails.
  #
  # Note that core extensions define <tt>Hash#with_indifferent_access</tt>:
  #
  #   rgb = { black: '#000000', white: '#FFFFFF' }.with_indifferent_access
  #
  # which may be handy.
  class HashWithIndifferentAccess < Hash
    # Returns +true+ so that <tt>Array#extract_options!</tt> finds members of
    # this class.
    def extractable_options?
      true
    end

    def with_indifferent_access
      dup
    end

    def nested_under_indifferent_access
      self
    end

    def initialize(constructor = {})
      if constructor.respond_to?(:to_hash)
        super()
        update(constructor)
      else
        super(constructor)
      end
    end

    def default(key = nil)
      if key.is_a?(Symbol) && include?(key = key.to_s)
        self[key]
      else
        super
      end
    end

    def self.new_from_hash_copying_default(hash)
      hash = hash.to_hash
      new(hash).tap do |new_hash|
        new_hash.default = hash.default
        new_hash.default_proc = hash.default_proc if hash.default_proc
      end
    end

    def self.[](*args)
      new.merge!(Hash[*args])
    end

    alias_method :regular_writer, :[]= unless method_defined?(:regular_writer)
    alias_method :regular_update, :update unless method_defined?(:regular_update)

    # Assigns a new value to the hash:
    #
    #   hash = ActiveSupport::HashWithIndifferentAccess.new
    #   hash[:key] = 'value'
    #
    # This value can be later fetched using either +:key+ or +'key'+.
    def []=(key, value)
      regular_writer(convert_key(key), convert_value(value, for: :assignment))
    end

    alias_method :store, :[]=

    # Updates the receiver in-place, merging in the hash passed as argument:
    #
    #   hash_1 = ActiveSupport::HashWithIndifferentAccess.new
    #   hash_1[:key] = 'value'
    #
    #   hash_2 = ActiveSupport::HashWithIndifferentAccess.new
    #   hash_2[:key] = 'New Value!'
    #
    #   hash_1.update(hash_2) # => {"key"=>"New Value!"}
    #
    # The argument can be either an
    # <tt>ActiveSupport::HashWithIndifferentAccess</tt> or a regular +Hash+.
    # In either case the merge respects the semantics of indifferent access.
    #
    # If the argument is a regular hash with keys +:key+ and +"key"+ only one
    # of the values end up in the receiver, but which one is unspecified.
    #
    # When given a block, the value for duplicated keys will be determined
    # by the result of invoking the block with the duplicated key, the value
    # in the receiver, and the value in +other_hash+. The rules for duplicated
    # keys follow the semantics of indifferent access:
    #
    #   hash_1[:key] = 10
    #   hash_2['key'] = 12
    #   hash_1.update(hash_2) { |key, old, new| old + new } # => {"key"=>22}
    def update(other_hash)
      if other_hash.is_a? HashWithIndifferentAccess
        super(other_hash)
      else
        other_hash.to_hash.each_pair do |key, value|
          if block_given? && key?(key)
            value = yield(convert_key(key), self[key], value)
          end
          regular_writer(convert_key(key), convert_value(value))
        end
        self
      end
    end

    alias_method :merge!, :update

    # Checks the hash for a key matching the argument passed in:
    #
    #   hash = ActiveSupport::HashWithIndifferentAccess.new
    #   hash['key'] = 'value'
    #   hash.key?(:key)  # => true
    #   hash.key?('key') # => true
    def key?(key)
      super(convert_key(key))
    end

    alias_method :include?, :key?
    alias_method :has_key?, :key?
    alias_method :member?, :key?

    # Same as <tt>Hash#fetch</tt> where the key passed as argument can be
    # either a string or a symbol:
    #
    #   counters = ActiveSupport::HashWithIndifferentAccess.new
    #   counters[:foo] = 1
    #
    #   counters.fetch('foo')          # => 1
    #   counters.fetch(:bar, 0)        # => 0
    #   counters.fetch(:bar) { |key| 0 } # => 0
    #   counters.fetch(:zoo)           # => KeyError: key not found: "zoo"
    def fetch(key, *extras)
      super(convert_key(key), *extras)
    end

    # Returns an array of the values at the specified indices:
    #
    #   hash = ActiveSupport::HashWithIndifferentAccess.new
    #   hash[:a] = 'x'
    #   hash[:b] = 'y'
    #   hash.values_at('a', 'b') # => ["x", "y"]
    def values_at(*indices)
      indices.collect { |key| self[convert_key(key)] }
    end

    # Returns a shallow copy of the hash.
    #
    #   hash = ActiveSupport::HashWithIndifferentAccess.new({ a: { b: 'b' } })
    #   dup  = hash.dup
    #   dup[:a][:c] = 'c'
    #
    #   hash[:a][:c] # => nil
    #   dup[:a][:c]  # => "c"
    def dup
      self.class.new(self).tap do |new_hash|
        new_hash.default = default
      end
    end

    # This method has the same semantics of +update+, except it does not
    # modify the receiver but rather returns a new hash with indifferent
    # access with the result of the merge.
    def merge(hash, &block)
      self.dup.update(hash, &block)
    end

    # Like +merge+ but the other way around: Merges the receiver into the
    # argument and returns a new hash with indifferent access as result:
    #
    #   hash = ActiveSupport::HashWithIndifferentAccess.new
    #   hash['a'] = nil
    #   hash.reverse_merge(a: 0, b: 1) # => {"a"=>nil, "b"=>1}
    def reverse_merge(other_hash)
      super(self.class.new_from_hash_copying_default(other_hash))
    end

    # Same semantics as +reverse_merge+ but modifies the receiver in-place.
    def reverse_merge!(other_hash)
      replace(reverse_merge( other_hash ))
    end

    # Replaces the contents of this hash with other_hash.
    #
    #   h = { "a" => 100, "b" => 200 }
    #   h.replace({ "c" => 300, "d" => 400 }) # => {"c"=>300, "d"=>400}
    def replace(other_hash)
      super(self.class.new_from_hash_copying_default(other_hash))
    end

    # Removes the specified key from the hash.
    def delete(key)
      super(convert_key(key))
    end

    def stringify_keys!; self end
    def deep_stringify_keys!; self end
    def stringify_keys; dup end
    def deep_stringify_keys; dup end
    undef :symbolize_keys!
    undef :deep_symbolize_keys!
    def symbolize_keys; to_hash.symbolize_keys! end
    def deep_symbolize_keys; to_hash.deep_symbolize_keys! end
    def to_options!; self end

    def select(*args, &block)
      dup.tap { |hash| hash.select!(*args, &block) }
    end

    def reject(*args, &block)
      dup.tap { |hash| hash.reject!(*args, &block) }
    end

    # Convert to a regular hash with string keys.
    def to_hash
      _new_hash = Hash.new(default)
      each do |key, value|
        _new_hash[key] = convert_value(value, for: :to_hash)
      end
      _new_hash
    end

    protected
      def convert_key(key)
        key.kind_of?(Symbol) ? key.to_s : key
      end

      def convert_value(value, options = {})
        if value.is_a? Hash
          if options[:for] == :to_hash
            value.to_hash
          else
            value.nested_under_indifferent_access
          end
        elsif value.is_a?(Array)
          if options[:for] != :assignment || value.frozen?
            value = value.dup
          end
          value.map! { |e| convert_value(e, options) }
        else
          value
        end
      end
  end
end

HashWithIndifferentAccess = ActiveSupport::HashWithIndifferentAccess
require 'active_support/core_ext/hash/deep_merge'
require 'active_support/core_ext/hash/except'
require 'active_support/core_ext/hash/slice'
begin
  require 'i18n'
rescue LoadError => e
  $stderr.puts "The i18n gem is not available. Please add it to your Gemfile and run bundle install"
  raise e
end
require 'active_support/lazy_load_hooks'

ActiveSupport.run_load_hooks(:i18n)
I18n.load_path << "#{File.dirname(__FILE__)}/locale/en.yml"
require "active_support"
require "active_support/file_update_checker"
require "active_support/core_ext/array/wrap"

module I18n
  class Railtie < Rails::Railtie
    config.i18n = ActiveSupport::OrderedOptions.new
    config.i18n.railties_load_path = []
    config.i18n.load_path = []
    config.i18n.fallbacks = ActiveSupport::OrderedOptions.new

    # Set the i18n configuration after initialization since a lot of
    # configuration is still usually done in application initializers.
    config.after_initialize do |app|
      I18n::Railtie.initialize_i18n(app)
    end

    # Trigger i18n config before any eager loading has happened
    # so it's ready if any classes require it when eager loaded.
    config.before_eager_load do |app|
      I18n::Railtie.initialize_i18n(app)
    end

  protected

    @i18n_inited = false

    # Setup i18n configuration.
    def self.initialize_i18n(app)
      return if @i18n_inited

      fallbacks = app.config.i18n.delete(:fallbacks)

      # Avoid issues with setting the default_locale by disabling available locales
      # check while configuring.
      enforce_available_locales = app.config.i18n.delete(:enforce_available_locales)
      enforce_available_locales = I18n.enforce_available_locales if enforce_available_locales.nil?
      I18n.enforce_available_locales = false

      app.config.i18n.each do |setting, value|
        case setting
        when :railties_load_path
          app.config.i18n.load_path.unshift(*value)
        when :load_path
          I18n.load_path += value
        else
          I18n.send("#{setting}=", value)
        end
      end

      init_fallbacks(fallbacks) if fallbacks && validate_fallbacks(fallbacks)

      # Restore available locales check so it will take place from now on.
      I18n.enforce_available_locales = enforce_available_locales

      reloader = ActiveSupport::FileUpdateChecker.new(I18n.load_path.dup){ I18n.reload! }
      app.reloaders << reloader
      ActionDispatch::Reloader.to_prepare do
        reloader.execute_if_updated
        # TODO: remove the following line as soon as the return value of
        # callbacks is ignored, that is, returning `false` does not
        # display a deprecation warning or halts the callback chain.
        true
      end
      reloader.execute

      @i18n_inited = true
    end

    def self.include_fallbacks_module
      I18n.backend.class.include(I18n::Backend::Fallbacks)
    end

    def self.init_fallbacks(fallbacks)
      include_fallbacks_module

      args = case fallbacks
      when ActiveSupport::OrderedOptions
        [*(fallbacks[:defaults] || []) << fallbacks[:map]].compact
      when Hash, Array
        Array.wrap(fallbacks)
      else # TrueClass
        []
      end

      I18n.fallbacks = I18n::Locale::Fallbacks.new(*args)
    end

    def self.validate_fallbacks(fallbacks)
      case fallbacks
      when ActiveSupport::OrderedOptions
        !fallbacks.empty?
      when TrueClass, Array, Hash
        true
      else
        raise "Unexpected fallback type #{fallbacks.inspect}"
      end
    end
  end
end
require 'active_support/inflector/inflections'

#--
# Defines the standard inflection rules. These are the starting point for
# new projects and are not considered complete. The current set of inflection
# rules is frozen. This means, we do not change them to become more complete.
# This is a safety measure to keep existing applications from breaking.
#++
module ActiveSupport
  Inflector.inflections(:en) do |inflect|
    inflect.plural(/$/, 's')
    inflect.plural(/s$/i, 's')
    inflect.plural(/^(ax|test)is$/i, '\1es')
    inflect.plural(/(octop|vir)us$/i, '\1i')
    inflect.plural(/(octop|vir)i$/i, '\1i')
    inflect.plural(/(alias|status)$/i, '\1es')
    inflect.plural(/(bu)s$/i, '\1ses')
    inflect.plural(/(buffal|tomat)o$/i, '\1oes')
    inflect.plural(/([ti])um$/i, '\1a')
    inflect.plural(/([ti])a$/i, '\1a')
    inflect.plural(/sis$/i, 'ses')
    inflect.plural(/(?:([^f])fe|([lr])f)$/i, '\1\2ves')
    inflect.plural(/(hive)$/i, '\1s')
    inflect.plural(/([^aeiouy]|qu)y$/i, '\1ies')
    inflect.plural(/(x|ch|ss|sh)$/i, '\1es')
    inflect.plural(/(matr|vert|ind)(?:ix|ex)$/i, '\1ices')
    inflect.plural(/^(m|l)ouse$/i, '\1ice')
    inflect.plural(/^(m|l)ice$/i, '\1ice')
    inflect.plural(/^(ox)$/i, '\1en')
    inflect.plural(/^(oxen)$/i, '\1')
    inflect.plural(/(quiz)$/i, '\1zes')

    inflect.singular(/s$/i, '')
    inflect.singular(/(ss)$/i, '\1')
    inflect.singular(/(n)ews$/i, '\1ews')
    inflect.singular(/([ti])a$/i, '\1um')
    inflect.singular(/((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$/i, '\1sis')
    inflect.singular(/(^analy)(sis|ses)$/i, '\1sis')
    inflect.singular(/([^f])ves$/i, '\1fe')
    inflect.singular(/(hive)s$/i, '\1')
    inflect.singular(/(tive)s$/i, '\1')
    inflect.singular(/([lr])ves$/i, '\1f')
    inflect.singular(/([^aeiouy]|qu)ies$/i, '\1y')
    inflect.singular(/(s)eries$/i, '\1eries')
    inflect.singular(/(m)ovies$/i, '\1ovie')
    inflect.singular(/(x|ch|ss|sh)es$/i, '\1')
    inflect.singular(/^(m|l)ice$/i, '\1ouse')
    inflect.singular(/(bus)(es)?$/i, '\1')
    inflect.singular(/(o)es$/i, '\1')
    inflect.singular(/(shoe)s$/i, '\1')
    inflect.singular(/(cris|test)(is|es)$/i, '\1is')
    inflect.singular(/^(a)x[ie]s$/i, '\1xis')
    inflect.singular(/(octop|vir)(us|i)$/i, '\1us')
    inflect.singular(/(alias|status)(es)?$/i, '\1')
    inflect.singular(/^(ox)en/i, '\1')
    inflect.singular(/(vert|ind)ices$/i, '\1ex')
    inflect.singular(/(matr)ices$/i, '\1ix')
    inflect.singular(/(quiz)zes$/i, '\1')
    inflect.singular(/(database)s$/i, '\1')

    inflect.irregular('person', 'people')
    inflect.irregular('man', 'men')
    inflect.irregular('child', 'children')
    inflect.irregular('sex', 'sexes')
    inflect.irregular('move', 'moves')
    inflect.irregular('zombie', 'zombies')

    inflect.uncountable(%w(equipment information rice money species series fish sheep jeans police))
  end
end
# in case active_support/inflector is required without the rest of active_support
require 'active_support/inflector/inflections'
require 'active_support/inflector/transliterate'
require 'active_support/inflector/methods'

require 'active_support/inflections'
require 'active_support/core_ext/string/inflections'
require 'thread_safe'
require 'active_support/core_ext/array/prepend_and_append'
require 'active_support/i18n'

module ActiveSupport
  module Inflector
    extend self

    # A singleton instance of this class is yielded by Inflector.inflections,
    # which can then be used to specify additional inflection rules. If passed
    # an optional locale, rules for other languages can be specified. The
    # default locale is <tt>:en</tt>. Only rules for English are provided.
    #
    #   ActiveSupport::Inflector.inflections(:en) do |inflect|
    #     inflect.plural /^(ox)$/i, '\1\2en'
    #     inflect.singular /^(ox)en/i, '\1'
    #
    #     inflect.irregular 'octopus', 'octopi'
    #
    #     inflect.uncountable 'equipment'
    #   end
    #
    # New rules are added at the top. So in the example above, the irregular
    # rule for octopus will now be the first of the pluralization and
    # singularization rules that is runs. This guarantees that your rules run
    # before any of the rules that may already have been loaded.
    class Inflections
      @__instance__ = ThreadSafe::Cache.new

      def self.instance(locale = :en)
        @__instance__[locale] ||= new
      end

      attr_reader :plurals, :singulars, :uncountables, :humans, :acronyms, :acronym_regex

      def initialize
        @plurals, @singulars, @uncountables, @humans, @acronyms, @acronym_regex = [], [], [], [], {}, /(?=a)b/
      end

      # Private, for the test suite.
      def initialize_dup(orig) # :nodoc:
        %w(plurals singulars uncountables humans acronyms acronym_regex).each do |scope|
          instance_variable_set("@#{scope}", orig.send(scope).dup)
        end
      end

      # Specifies a new acronym. An acronym must be specified as it will appear
      # in a camelized string. An underscore string that contains the acronym
      # will retain the acronym when passed to +camelize+, +humanize+, or
      # +titleize+. A camelized string that contains the acronym will maintain
      # the acronym when titleized or humanized, and will convert the acronym
      # into a non-delimited single lowercase word when passed to +underscore+.
      #
      #   acronym 'HTML'
      #   titleize 'html'     # => 'HTML'
      #   camelize 'html'     # => 'HTML'
      #   underscore 'MyHTML' # => 'my_html'
      #
      # The acronym, however, must occur as a delimited unit and not be part of
      # another word for conversions to recognize it:
      #
      #   acronym 'HTTP'
      #   camelize 'my_http_delimited' # => 'MyHTTPDelimited'
      #   camelize 'https'             # => 'Https', not 'HTTPs'
      #   underscore 'HTTPS'           # => 'http_s', not 'https'
      #
      #   acronym 'HTTPS'
      #   camelize 'https'   # => 'HTTPS'
      #   underscore 'HTTPS' # => 'https'
      #
      # Note: Acronyms that are passed to +pluralize+ will no longer be
      # recognized, since the acronym will not occur as a delimited unit in the
      # pluralized result. To work around this, you must specify the pluralized
      # form as an acronym as well:
      #
      #    acronym 'API'
      #    camelize(pluralize('api')) # => 'Apis'
      #
      #    acronym 'APIs'
      #    camelize(pluralize('api')) # => 'APIs'
      #
      # +acronym+ may be used to specify any word that contains an acronym or
      # otherwise needs to maintain a non-standard capitalization. The only
      # restriction is that the word must begin with a capital letter.
      #
      #   acronym 'RESTful'
      #   underscore 'RESTful'           # => 'restful'
      #   underscore 'RESTfulController' # => 'restful_controller'
      #   titleize 'RESTfulController'   # => 'RESTful Controller'
      #   camelize 'restful'             # => 'RESTful'
      #   camelize 'restful_controller'  # => 'RESTfulController'
      #
      #   acronym 'McDonald'
      #   underscore 'McDonald' # => 'mcdonald'
      #   camelize 'mcdonald'   # => 'McDonald'
      def acronym(word)
        @acronyms[word.downcase] = word
        @acronym_regex = /#{@acronyms.values.join("|")}/
      end

      # Specifies a new pluralization rule and its replacement. The rule can
      # either be a string or a regular expression. The replacement should
      # always be a string that may include references to the matched data from
      # the rule.
      def plural(rule, replacement)
        @uncountables.delete(rule) if rule.is_a?(String)
        @uncountables.delete(replacement)
        @plurals.prepend([rule, replacement])
      end

      # Specifies a new singularization rule and its replacement. The rule can
      # either be a string or a regular expression. The replacement should
      # always be a string that may include references to the matched data from
      # the rule.
      def singular(rule, replacement)
        @uncountables.delete(rule) if rule.is_a?(String)
        @uncountables.delete(replacement)
        @singulars.prepend([rule, replacement])
      end

      # Specifies a new irregular that applies to both pluralization and
      # singularization at the same time. This can only be used for strings, not
      # regular expressions. You simply pass the irregular in singular and
      # plural form.
      #
      #   irregular 'octopus', 'octopi'
      #   irregular 'person', 'people'
      def irregular(singular, plural)
        @uncountables.delete(singular)
        @uncountables.delete(plural)

        s0 = singular[0]
        srest = singular[1..-1]

        p0 = plural[0]
        prest = plural[1..-1]

        if s0.upcase == p0.upcase
          plural(/(#{s0})#{srest}$/i, '\1' + prest)
          plural(/(#{p0})#{prest}$/i, '\1' + prest)

          singular(/(#{s0})#{srest}$/i, '\1' + srest)
          singular(/(#{p0})#{prest}$/i, '\1' + srest)
        else
          plural(/#{s0.upcase}(?i)#{srest}$/,   p0.upcase   + prest)
          plural(/#{s0.downcase}(?i)#{srest}$/, p0.downcase + prest)
          plural(/#{p0.upcase}(?i)#{prest}$/,   p0.upcase   + prest)
          plural(/#{p0.downcase}(?i)#{prest}$/, p0.downcase + prest)

          singular(/#{s0.upcase}(?i)#{srest}$/,   s0.upcase   + srest)
          singular(/#{s0.downcase}(?i)#{srest}$/, s0.downcase + srest)
          singular(/#{p0.upcase}(?i)#{prest}$/,   s0.upcase   + srest)
          singular(/#{p0.downcase}(?i)#{prest}$/, s0.downcase + srest)
        end
      end

      # Specifies words that are uncountable and should not be inflected.
      #
      #   uncountable 'money'
      #   uncountable 'money', 'information'
      #   uncountable %w( money information rice )
      def uncountable(*words)
        @uncountables += words.flatten.map(&:downcase)
      end

      # Specifies a humanized form of a string by a regular expression rule or
      # by a string mapping. When using a regular expression based replacement,
      # the normal humanize formatting is called after the replacement. When a
      # string is used, the human form should be specified as desired (example:
      # 'The name', not 'the_name').
      #
      #   human /_cnt$/i, '\1_count'
      #   human 'legacy_col_person_name', 'Name'
      def human(rule, replacement)
        @humans.prepend([rule, replacement])
      end

      # Clears the loaded inflections within a given scope (default is
      # <tt>:all</tt>). Give the scope as a symbol of the inflection type, the
      # options are: <tt>:plurals</tt>, <tt>:singulars</tt>, <tt>:uncountables</tt>,
      # <tt>:humans</tt>.
      #
      #   clear :all
      #   clear :plurals
      def clear(scope = :all)
        case scope
          when :all
            @plurals, @singulars, @uncountables, @humans = [], [], [], []
          else
            instance_variable_set "@#{scope}", []
        end
      end
    end

    # Yields a singleton instance of Inflector::Inflections so you can specify
    # additional inflector rules. If passed an optional locale, rules for other
    # languages can be specified. If not specified, defaults to <tt>:en</tt>.
    # Only rules for English are provided.
    #
    #   ActiveSupport::Inflector.inflections(:en) do |inflect|
    #     inflect.uncountable 'rails'
    #   end
    def inflections(locale = :en)
      if block_given?
        yield Inflections.instance(locale)
      else
        Inflections.instance(locale)
      end
    end
  end
end
# encoding: utf-8

require 'active_support/inflections'

module ActiveSupport
  # The Inflector transforms words from singular to plural, class names to table
  # names, modularized class names to ones without, and class names to foreign
  # keys. The default inflections for pluralization, singularization, and
  # uncountable words are kept in inflections.rb.
  #
  # The Rails core team has stated patches for the inflections library will not
  # be accepted in order to avoid breaking legacy applications which may be
  # relying on errant inflections. If you discover an incorrect inflection and
  # require it for your application or wish to define rules for languages other
  # than English, please correct or add them yourself (explained below).
  module Inflector
    extend self

    # Returns the plural form of the word in the string.
    #
    # If passed an optional +locale+ parameter, the word will be
    # pluralized using rules defined for that language. By default,
    # this parameter is set to <tt>:en</tt>.
    #
    #   pluralize('post')             # => "posts"
    #   pluralize('octopus')          # => "octopi"
    #   pluralize('sheep')            # => "sheep"
    #   pluralize('words')            # => "words"
    #   pluralize('CamelOctopus')     # => "CamelOctopi"
    #   pluralize('ley', :es)         # => "leyes"
    def pluralize(word, locale = :en)
      apply_inflections(word, inflections(locale).plurals)
    end

    # The reverse of #pluralize, returns the singular form of a word in a
    # string.
    #
    # If passed an optional +locale+ parameter, the word will be
    # singularized using rules defined for that language. By default,
    # this parameter is set to <tt>:en</tt>.
    #
    #   singularize('posts')            # => "post"
    #   singularize('octopi')           # => "octopus"
    #   singularize('sheep')            # => "sheep"
    #   singularize('word')             # => "word"
    #   singularize('CamelOctopi')      # => "CamelOctopus"
    #   singularize('leyes', :es)       # => "ley"
    def singularize(word, locale = :en)
      apply_inflections(word, inflections(locale).singulars)
    end

    # Converts strings to UpperCamelCase.
    # If the +uppercase_first_letter+ parameter is set to false, then produces
    # lowerCamelCase.
    #
    # Also converts '/' to '::' which is useful for converting
    # paths to namespaces.
    #
    #   camelize('active_model')                # => "ActiveModel"
    #   camelize('active_model', false)         # => "activeModel"
    #   camelize('active_model/errors')         # => "ActiveModel::Errors"
    #   camelize('active_model/errors', false)  # => "activeModel::Errors"
    #
    # As a rule of thumb you can think of +camelize+ as the inverse of
    # #underscore, though there are cases where that does not hold:
    #
    #   camelize(underscore('SSLError'))        # => "SslError"
    def camelize(term, uppercase_first_letter = true)
      string = term.to_s
      if uppercase_first_letter
        string = string.sub(/^[a-z\d]*/) { inflections.acronyms[$&] || $&.capitalize }
      else
        string = string.sub(/^(?:#{inflections.acronym_regex}(?=\b|[A-Z_])|\w)/) { $&.downcase }
      end
      string.gsub!(/(?:_|(\/))([a-z\d]*)/i) { "#{$1}#{inflections.acronyms[$2] || $2.capitalize}" }
      string.gsub!('/'.freeze, '::'.freeze)
      string
    end

    # Makes an underscored, lowercase form from the expression in the string.
    #
    # Changes '::' to '/' to convert namespaces to paths.
    #
    #   underscore('ActiveModel')         # => "active_model"
    #   underscore('ActiveModel::Errors') # => "active_model/errors"
    #
    # As a rule of thumb you can think of +underscore+ as the inverse of
    # #camelize, though there are cases where that does not hold:
    #
    #   camelize(underscore('SSLError'))  # => "SslError"
    def underscore(camel_cased_word)
      return camel_cased_word unless camel_cased_word =~ /[A-Z-]|::/
      word = camel_cased_word.to_s.gsub('::'.freeze, '/'.freeze)
      word.gsub!(/(?:(?<=([A-Za-z\d]))|\b)(#{inflections.acronym_regex})(?=\b|[^a-z])/) { "#{$1 && '_'}#{$2.downcase}" }
      word.gsub!(/([A-Z\d]+)([A-Z][a-z])/,'\1_\2')
      word.gsub!(/([a-z\d])([A-Z])/,'\1_\2')
      word.tr!("-", "_")
      word.downcase!
      word
    end

    # Tweaks an attribute name for display to end users.
    #
    # Specifically, performs these transformations:
    #
    # * Applies human inflection rules to the argument.
    # * Deletes leading underscores, if any.
    # * Removes a "_id" suffix if present.
    # * Replaces underscores with spaces, if any.
    # * Downcases all words except acronyms.
    # * Capitalizes the first word.
    #
    # The capitalization of the first word can be turned off by setting the
    # +:capitalize+ option to false (default is true).
    #
    #   humanize('employee_salary')              # => "Employee salary"
    #   humanize('author_id')                    # => "Author"
    #   humanize('author_id', capitalize: false) # => "author"
    #   humanize('_id')                          # => "Id"
    #
    # If "SSL" was defined to be an acronym:
    #
    #   humanize('ssl_error') # => "SSL error"
    #
    def humanize(lower_case_and_underscored_word, options = {})
      result = lower_case_and_underscored_word.to_s.dup

      inflections.humans.each { |(rule, replacement)| break if result.sub!(rule, replacement) }

      result.sub!(/\A_+/, '')
      result.sub!(/_id\z/, '')
      result.tr!('_', ' ')

      result.gsub!(/([a-z\d]*)/i) do |match|
        "#{inflections.acronyms[match] || match.downcase}"
      end

      if options.fetch(:capitalize, true)
        result.sub!(/\A\w/) { |match| match.upcase }
      end

      result
    end

    # Capitalizes all the words and replaces some characters in the string to
    # create a nicer looking title. +titleize+ is meant for creating pretty
    # output. It is not used in the Rails internals.
    #
    # +titleize+ is also aliased as +titlecase+.
    #
    #   titleize('man from the boondocks')   # => "Man From The Boondocks"
    #   titleize('x-men: the last stand')    # => "X Men: The Last Stand"
    #   titleize('TheManWithoutAPast')       # => "The Man Without A Past"
    #   titleize('raiders_of_the_lost_ark')  # => "Raiders Of The Lost Ark"
    def titleize(word)
      humanize(underscore(word)).gsub(/\b(?<!['’`])[a-z]/) { $&.capitalize }
    end

    # Creates the name of a table like Rails does for models to table names.
    # This method uses the #pluralize method on the last word in the string.
    #
    #   tableize('RawScaledScorer') # => "raw_scaled_scorers"
    #   tableize('egg_and_ham')     # => "egg_and_hams"
    #   tableize('fancyCategory')   # => "fancy_categories"
    def tableize(class_name)
      pluralize(underscore(class_name))
    end

    # Creates a class name from a plural table name like Rails does for table
    # names to models. Note that this returns a string and not a Class (To
    # convert to an actual class follow +classify+ with #constantize).
    #
    #   classify('egg_and_hams') # => "EggAndHam"
    #   classify('posts')        # => "Post"
    #
    # Singular names are not handled correctly:
    #
    #   classify('calculus')     # => "Calculu"
    def classify(table_name)
      # strip out any leading schema name
      camelize(singularize(table_name.to_s.sub(/.*\./, '')))
    end

    # Replaces underscores with dashes in the string.
    #
    #   dasherize('puni_puni') # => "puni-puni"
    def dasherize(underscored_word)
      underscored_word.tr('_', '-')
    end

    # Removes the module part from the expression in the string.
    #
    #   demodulize('ActiveRecord::CoreExtensions::String::Inflections') # => "Inflections"
    #   demodulize('Inflections')                                       # => "Inflections"
    #   demodulize('::Inflections')                                     # => "Inflections"
    #   demodulize('')                                                  # => ""
    #
    # See also #deconstantize.
    def demodulize(path)
      path = path.to_s
      if i = path.rindex('::')
        path[(i+2)..-1]
      else
        path
      end
    end

    # Removes the rightmost segment from the constant expression in the string.
    #
    #   deconstantize('Net::HTTP')   # => "Net"
    #   deconstantize('::Net::HTTP') # => "::Net"
    #   deconstantize('String')      # => ""
    #   deconstantize('::String')    # => ""
    #   deconstantize('')            # => ""
    #
    # See also #demodulize.
    def deconstantize(path)
      path.to_s[0, path.rindex('::') || 0] # implementation based on the one in facets' Module#spacename
    end

    # Creates a foreign key name from a class name.
    # +separate_class_name_and_id_with_underscore+ sets whether
    # the method should put '_' between the name and 'id'.
    #
    #   foreign_key('Message')        # => "message_id"
    #   foreign_key('Message', false) # => "messageid"
    #   foreign_key('Admin::Post')    # => "post_id"
    def foreign_key(class_name, separate_class_name_and_id_with_underscore = true)
      underscore(demodulize(class_name)) + (separate_class_name_and_id_with_underscore ? "_id" : "id")
    end

    # Tries to find a constant with the name specified in the argument string.
    #
    #   'Module'.constantize   # => Module
    #   'Foo::Bar'.constantize # => Foo::Bar
    #
    # The name is assumed to be the one of a top-level constant, no matter
    # whether it starts with "::" or not. No lexical context is taken into
    # account:
    #
    #   C = 'outside'
    #   module M
    #     C = 'inside'
    #     C               # => 'inside'
    #     'C'.constantize # => 'outside', same as ::C
    #   end
    #
    # NameError is raised when the name is not in CamelCase or the constant is
    # unknown.
    def constantize(camel_cased_word)
      names = camel_cased_word.split('::')

      # Trigger a built-in NameError exception including the ill-formed constant in the message.
      Object.const_get(camel_cased_word) if names.empty?

      # Remove the first blank element in case of '::ClassName' notation.
      names.shift if names.size > 1 && names.first.empty?

      names.inject(Object) do |constant, name|
        if constant == Object
          constant.const_get(name)
        else
          candidate = constant.const_get(name)
          next candidate if constant.const_defined?(name, false)
          next candidate unless Object.const_defined?(name)

          # Go down the ancestors to check if it is owned directly. The check
          # stops when we reach Object or the end of ancestors tree.
          constant = constant.ancestors.inject do |const, ancestor|
            break const    if ancestor == Object
            break ancestor if ancestor.const_defined?(name, false)
            const
          end

          # owner is in Object, so raise
          constant.const_get(name, false)
        end
      end
    end

    # Tries to find a constant with the name specified in the argument string.
    #
    #   safe_constantize('Module')   # => Module
    #   safe_constantize('Foo::Bar') # => Foo::Bar
    #
    # The name is assumed to be the one of a top-level constant, no matter
    # whether it starts with "::" or not. No lexical context is taken into
    # account:
    #
    #   C = 'outside'
    #   module M
    #     C = 'inside'
    #     C                     # => 'inside'
    #     safe_constantize('C') # => 'outside', same as ::C
    #   end
    #
    # +nil+ is returned when the name is not in CamelCase or the constant (or
    # part of it) is unknown.
    #
    #   safe_constantize('blargle')                  # => nil
    #   safe_constantize('UnknownModule')            # => nil
    #   safe_constantize('UnknownModule::Foo::Bar')  # => nil
    def safe_constantize(camel_cased_word)
      constantize(camel_cased_word)
    rescue NameError => e
      raise if e.name && !(camel_cased_word.to_s.split("::").include?(e.name.to_s) ||
        e.name.to_s == camel_cased_word.to_s)
    rescue ArgumentError => e
      raise unless e.message =~ /not missing constant #{const_regexp(camel_cased_word)}\!$/
    end

    # Returns the suffix that should be added to a number to denote the position
    # in an ordered sequence such as 1st, 2nd, 3rd, 4th.
    #
    #   ordinal(1)     # => "st"
    #   ordinal(2)     # => "nd"
    #   ordinal(1002)  # => "nd"
    #   ordinal(1003)  # => "rd"
    #   ordinal(-11)   # => "th"
    #   ordinal(-1021) # => "st"
    def ordinal(number)
      abs_number = number.to_i.abs

      if (11..13).include?(abs_number % 100)
        "th"
      else
        case abs_number % 10
          when 1; "st"
          when 2; "nd"
          when 3; "rd"
          else    "th"
        end
      end
    end

    # Turns a number into an ordinal string used to denote the position in an
    # ordered sequence such as 1st, 2nd, 3rd, 4th.
    #
    #   ordinalize(1)     # => "1st"
    #   ordinalize(2)     # => "2nd"
    #   ordinalize(1002)  # => "1002nd"
    #   ordinalize(1003)  # => "1003rd"
    #   ordinalize(-11)   # => "-11th"
    #   ordinalize(-1021) # => "-1021st"
    def ordinalize(number)
      "#{number}#{ordinal(number)}"
    end

    private

    # Mounts a regular expression, returned as a string to ease interpolation,
    # that will match part by part the given constant.
    #
    #   const_regexp("Foo::Bar::Baz") # => "Foo(::Bar(::Baz)?)?"
    #   const_regexp("::")            # => "::"
    def const_regexp(camel_cased_word) #:nodoc:
      parts = camel_cased_word.split("::")

      return Regexp.escape(camel_cased_word) if parts.blank?

      last  = parts.pop

      parts.reverse.inject(last) do |acc, part|
        part.empty? ? acc : "#{part}(::#{acc})?"
      end
    end

    # Applies inflection rules for +singularize+ and +pluralize+.
    #
    #  apply_inflections('post', inflections.plurals)    # => "posts"
    #  apply_inflections('posts', inflections.singulars) # => "post"
    def apply_inflections(word, rules)
      result = word.to_s.dup

      if word.empty? || inflections.uncountables.include?(result.downcase[/\b\w+\Z/])
        result
      else
        rules.each { |(rule, replacement)| break if result.sub!(rule, replacement) }
        result
      end
    end
  end
end
# encoding: utf-8
require 'active_support/core_ext/string/multibyte'
require 'active_support/i18n'

module ActiveSupport
  module Inflector

    # Replaces non-ASCII characters with an ASCII approximation, or if none
    # exists, a replacement character which defaults to "?".
    #
    #    transliterate('Ærøskøbing')
    #    # => "AEroskobing"
    #
    # Default approximations are provided for Western/Latin characters,
    # e.g, "ø", "ñ", "é", "ß", etc.
    #
    # This method is I18n aware, so you can set up custom approximations for a
    # locale. This can be useful, for example, to transliterate German's "ü"
    # and "ö" to "ue" and "oe", or to add support for transliterating Russian
    # to ASCII.
    #
    # In order to make your custom transliterations available, you must set
    # them as the <tt>i18n.transliterate.rule</tt> i18n key:
    #
    #   # Store the transliterations in locales/de.yml
    #   i18n:
    #     transliterate:
    #       rule:
    #         ü: "ue"
    #         ö: "oe"
    #
    #   # Or set them using Ruby
    #   I18n.backend.store_translations(:de, i18n: {
    #     transliterate: {
    #       rule: {
    #         'ü' => 'ue',
    #         'ö' => 'oe'
    #       }
    #     }
    #   })
    #
    # The value for <tt>i18n.transliterate.rule</tt> can be a simple Hash that
    # maps characters to ASCII approximations as shown above, or, for more
    # complex requirements, a Proc:
    #
    #   I18n.backend.store_translations(:de, i18n: {
    #     transliterate: {
    #       rule: ->(string) { MyTransliterator.transliterate(string) }
    #     }
    #   })
    #
    # Now you can have different transliterations for each locale:
    #
    #   I18n.locale = :en
    #   transliterate('Jürgen')
    #   # => "Jurgen"
    #
    #   I18n.locale = :de
    #   transliterate('Jürgen')
    #   # => "Juergen"
    def transliterate(string, replacement = "?")
      I18n.transliterate(ActiveSupport::Multibyte::Unicode.normalize(
        ActiveSupport::Multibyte::Unicode.tidy_bytes(string), :c),
          :replacement => replacement)
    end

    # Replaces special characters in a string so that it may be used as part of
    # a 'pretty' URL.
    #
    #   parameterize("Donald E. Knuth") # => "donald-e-knuth"
    #   parameterize("^trés|Jolie-- ")  # => "tres-jolie"
    def parameterize(string, sep = '-')
      # replace accented chars with their ascii equivalents
      parameterized_string = transliterate(string)
      # Turn unwanted chars into the separator
      parameterized_string.gsub!(/[^a-z0-9\-_]+/i, sep)
      unless sep.nil? || sep.empty?
        re_sep = Regexp.escape(sep)
        # No more than one of the separator in a row.
        parameterized_string.gsub!(/#{re_sep}{2,}/, sep)
        # Remove leading/trailing separator.
        parameterized_string.gsub!(/^#{re_sep}|#{re_sep}$/i, '')
      end
      parameterized_string.downcase
    end
  end
end
require 'active_support/json/decoding'
require 'active_support/json/encoding'
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/core_ext/module/delegation'
require 'json'

module ActiveSupport
  # Look for and parse json strings that look like ISO 8601 times.
  mattr_accessor :parse_json_times

  module JSON
    # matches YAML-formatted dates
    DATE_REGEX = /^(?:\d{4}-\d{2}-\d{2}|\d{4}-\d{1,2}-\d{1,2}[T \t]+\d{1,2}:\d{2}:\d{2}(\.[0-9]*)?(([ \t]*)Z|[-+]\d{2}?(:\d{2})?))$/

    class << self
      # Parses a JSON string (JavaScript Object Notation) into a hash.
      # See http://www.json.org for more info.
      #
      #   ActiveSupport::JSON.decode("{\"team\":\"rails\",\"players\":\"36\"}")
      #   => {"team" => "rails", "players" => "36"}
      def decode(json)
        data = ::JSON.parse(json, quirks_mode: true)

        if ActiveSupport.parse_json_times
          convert_dates_from(data)
        else
          data
        end
      end

      # Returns the class of the error that will be raised when there is an
      # error in decoding JSON. Using this method means you won't directly
      # depend on the ActiveSupport's JSON implementation, in case it changes
      # in the future.
      #
      #   begin
      #     obj = ActiveSupport::JSON.decode(some_string)
      #   rescue ActiveSupport::JSON.parse_error
      #     Rails.logger.warn("Attempted to decode invalid JSON: #{some_string}")
      #   end
      def parse_error
        ::JSON::ParserError
      end

      private

      def convert_dates_from(data)
        case data
        when nil
          nil
        when DATE_REGEX
          begin
            DateTime.parse(data)
          rescue ArgumentError
            data
          end
        when Array
          data.map! { |d| convert_dates_from(d) }
        when Hash
          data.each do |key, value|
            data[key] = convert_dates_from(value)
          end
        else
          data
        end
      end
    end
  end
end
require 'active_support/core_ext/object/json'
require 'active_support/core_ext/module/delegation'

module ActiveSupport
  class << self
    delegate :use_standard_json_time_format, :use_standard_json_time_format=,
      :time_precision, :time_precision=,
      :escape_html_entities_in_json, :escape_html_entities_in_json=,
      :json_encoder, :json_encoder=,
      :to => :'ActiveSupport::JSON::Encoding'
  end

  module JSON
    # Dumps objects in JSON (JavaScript Object Notation).
    # See http://www.json.org for more info.
    #
    #   ActiveSupport::JSON.encode({ team: 'rails', players: '36' })
    #   # => "{\"team\":\"rails\",\"players\":\"36\"}"
    def self.encode(value, options = nil)
      Encoding.json_encoder.new(options).encode(value)
    end

    module Encoding #:nodoc:
      class JSONGemEncoder #:nodoc:
        attr_reader :options

        def initialize(options = nil)
          @options = options || {}
        end

        # Encode the given object into a JSON string
        def encode(value)
          stringify jsonify value.as_json(options.dup)
        end

        private
          # Rails does more escaping than the JSON gem natively does (we
          # escape \u2028 and \u2029 and optionally >, <, & to work around
          # certain browser problems).
          ESCAPED_CHARS = {
            "\u2028" => '\u2028',
            "\u2029" => '\u2029',
            '>'      => '\u003e',
            '<'      => '\u003c',
            '&'      => '\u0026',
            }

          ESCAPE_REGEX_WITH_HTML_ENTITIES = /[\u2028\u2029><&]/u
          ESCAPE_REGEX_WITHOUT_HTML_ENTITIES = /[\u2028\u2029]/u

          # This class wraps all the strings we see and does the extra escaping
          class EscapedString < String #:nodoc:
            def to_json(*)
              if Encoding.escape_html_entities_in_json
                super.gsub ESCAPE_REGEX_WITH_HTML_ENTITIES, ESCAPED_CHARS
              else
                super.gsub ESCAPE_REGEX_WITHOUT_HTML_ENTITIES, ESCAPED_CHARS
              end
            end
          end

          # Mark these as private so we don't leak encoding-specific constructs
          private_constant :ESCAPED_CHARS, :ESCAPE_REGEX_WITH_HTML_ENTITIES,
            :ESCAPE_REGEX_WITHOUT_HTML_ENTITIES, :EscapedString

          # Convert an object into a "JSON-ready" representation composed of
          # primitives like Hash, Array, String, Numeric, and true/false/nil.
          # Recursively calls #as_json to the object to recursively build a
          # fully JSON-ready object.
          #
          # This allows developers to implement #as_json without having to
          # worry about what base types of objects they are allowed to return
          # or having to remember to call #as_json recursively.
          #
          # Note: the +options+ hash passed to +object.to_json+ is only passed
          # to +object.as_json+, not any of this method's recursive +#as_json+
          # calls.
          def jsonify(value)
            case value
            when String
              EscapedString.new(value)
            when Numeric, NilClass, TrueClass, FalseClass
              value
            when Hash
              Hash[value.map { |k, v| [jsonify(k), jsonify(v)] }]
            when Array
              value.map { |v| jsonify(v) }
            else
              jsonify value.as_json
            end
          end

          # Encode a "jsonified" Ruby data structure using the JSON gem
          def stringify(jsonified)
            ::JSON.generate(jsonified, quirks_mode: true, max_nesting: false)
          end
      end

      class << self
        # If true, use ISO 8601 format for dates and times. Otherwise, fall back
        # to the Active Support legacy format.
        attr_accessor :use_standard_json_time_format

        # If true, encode >, <, & as escaped unicode sequences (e.g. > as \u003e)
        # as a safety measure.
        attr_accessor :escape_html_entities_in_json

        # Sets the precision of encoded time values.
        # Defaults to 3 (equivalent to millisecond precision)
        attr_accessor :time_precision

        # Sets the encoder used by Rails to encode Ruby objects into JSON strings
        # in +Object#to_json+ and +ActiveSupport::JSON.encode+.
        attr_accessor :json_encoder
      end

      self.use_standard_json_time_format = true
      self.escape_html_entities_in_json  = true
      self.json_encoder = JSONGemEncoder
      self.time_precision = 3
    end
  end
end
require 'thread_safe'
require 'openssl'

module ActiveSupport
  # KeyGenerator is a simple wrapper around OpenSSL's implementation of PBKDF2
  # It can be used to derive a number of keys for various purposes from a given secret.
  # This lets Rails applications have a single secure secret, but avoid reusing that
  # key in multiple incompatible contexts.
  class KeyGenerator
    def initialize(secret, options = {})
      @secret = secret
      # The default iterations are higher than required for our key derivation uses
      # on the off chance someone uses this for password storage
      @iterations = options[:iterations] || 2**16
    end

    # Returns a derived key suitable for use.  The default key_size is chosen
    # to be compatible with the default settings of ActiveSupport::MessageVerifier.
    # i.e. OpenSSL::Digest::SHA1#block_length
    def generate_key(salt, key_size=64)
      OpenSSL::PKCS5.pbkdf2_hmac_sha1(@secret, salt, @iterations, key_size)
    end
  end

  # CachingKeyGenerator is a wrapper around KeyGenerator which allows users to avoid
  # re-executing the key generation process when it's called using the same salt and
  # key_size
  class CachingKeyGenerator
    def initialize(key_generator)
      @key_generator = key_generator
      @cache_keys = ThreadSafe::Cache.new
    end

    # Returns a derived key suitable for use.  The default key_size is chosen
    # to be compatible with the default settings of ActiveSupport::MessageVerifier.
    # i.e. OpenSSL::Digest::SHA1#block_length
    def generate_key(salt, key_size=64)
      @cache_keys["#{salt}#{key_size}"] ||= @key_generator.generate_key(salt, key_size)
    end
  end

  class LegacyKeyGenerator # :nodoc:
    SECRET_MIN_LENGTH = 30 # Characters

    def initialize(secret)
      ensure_secret_secure(secret)
      @secret = secret
    end

    def generate_key(salt)
      @secret
    end

    private

    # To prevent users from using something insecure like "Password" we make sure that the
    # secret they've provided is at least 30 characters in length.
    def ensure_secret_secure(secret)
      if secret.blank?
        raise ArgumentError, "A secret is required to generate an integrity hash " \
          "for cookie session data. Set a secret_key_base of at least " \
          "#{SECRET_MIN_LENGTH} characters in config/secrets.yml."
      end

      if secret.length < SECRET_MIN_LENGTH
        raise ArgumentError, "Secret should be something secure, " \
          "like \"#{SecureRandom.hex(16)}\". The value you " \
          "provided, \"#{secret}\", is shorter than the minimum length " \
          "of #{SECRET_MIN_LENGTH} characters."
      end
    end
  end
end
module ActiveSupport
  # lazy_load_hooks allows Rails to lazily load a lot of components and thus
  # making the app boot faster. Because of this feature now there is no need to
  # require <tt>ActiveRecord::Base</tt> at boot time purely to apply
  # configuration. Instead a hook is registered that applies configuration once
  # <tt>ActiveRecord::Base</tt> is loaded. Here <tt>ActiveRecord::Base</tt> is
  # used as example but this feature can be applied elsewhere too.
  #
  # Here is an example where +on_load+ method is called to register a hook.
  #
  #   initializer 'active_record.initialize_timezone' do
  #     ActiveSupport.on_load(:active_record) do
  #       self.time_zone_aware_attributes = true
  #       self.default_timezone = :utc
  #     end
  #   end
  #
  # When the entirety of +activerecord/lib/active_record/base.rb+ has been
  # evaluated then +run_load_hooks+ is invoked. The very last line of
  # +activerecord/lib/active_record/base.rb+ is:
  #
  #   ActiveSupport.run_load_hooks(:active_record, ActiveRecord::Base)
  @load_hooks = Hash.new { |h,k| h[k] = [] }
  @loaded = Hash.new { |h,k| h[k] = [] }

  def self.on_load(name, options = {}, &block)
    @loaded[name].each do |base|
      execute_hook(base, options, block)
    end

    @load_hooks[name] << [block, options]
  end

  def self.execute_hook(base, options, block)
    if options[:yield]
      block.call(base)
    else
      base.instance_eval(&block)
    end
  end

  def self.run_load_hooks(name, base = Object)
    @loaded[name] << base
    @load_hooks[name].each do |hook, options|
      execute_hook(base, options, hook)
    end
  end
end
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/core_ext/class/attribute'
require 'active_support/subscriber'

module ActiveSupport
  # ActiveSupport::LogSubscriber is an object set to consume
  # ActiveSupport::Notifications with the sole purpose of logging them.
  # The log subscriber dispatches notifications to a registered object based
  # on its given namespace.
  #
  # An example would be Active Record log subscriber responsible for logging
  # queries:
  #
  #   module ActiveRecord
  #     class LogSubscriber < ActiveSupport::LogSubscriber
  #       def sql(event)
  #         "#{event.payload[:name]} (#{event.duration}) #{event.payload[:sql]}"
  #       end
  #     end
  #   end
  #
  # And it's finally registered as:
  #
  #   ActiveRecord::LogSubscriber.attach_to :active_record
  #
  # Since we need to know all instance methods before attaching the log
  # subscriber, the line above should be called after your
  # <tt>ActiveRecord::LogSubscriber</tt> definition.
  #
  # After configured, whenever a "sql.active_record" notification is published,
  # it will properly dispatch the event (ActiveSupport::Notifications::Event) to
  # the sql method.
  #
  # Log subscriber also has some helpers to deal with logging and automatically
  # flushes all logs when the request finishes (via action_dispatch.callback
  # notification) in a Rails environment.
  class LogSubscriber < Subscriber
    # Embed in a String to clear all previous ANSI sequences.
    CLEAR   = "\e[0m"
    BOLD    = "\e[1m"

    # Colors
    BLACK   = "\e[30m"
    RED     = "\e[31m"
    GREEN   = "\e[32m"
    YELLOW  = "\e[33m"
    BLUE    = "\e[34m"
    MAGENTA = "\e[35m"
    CYAN    = "\e[36m"
    WHITE   = "\e[37m"

    mattr_accessor :colorize_logging
    self.colorize_logging = true

    class << self
      def logger
        @logger ||= if defined?(Rails) && Rails.respond_to?(:logger)
          Rails.logger
        end
      end

      attr_writer :logger

      def log_subscribers
        subscribers
      end

      # Flush all log_subscribers' logger.
      def flush_all!
        logger.flush if logger.respond_to?(:flush)
      end
    end

    def logger
      LogSubscriber.logger
    end

    def start(name, id, payload)
      super if logger
    end

    def finish(name, id, payload)
      super if logger
    rescue Exception => e
      logger.error "Could not log #{name.inspect} event. #{e.class}: #{e.message} #{e.backtrace}"
    end

  protected

    %w(info debug warn error fatal unknown).each do |level|
      class_eval <<-METHOD, __FILE__, __LINE__ + 1
        def #{level}(progname = nil, &block)
          logger.#{level}(progname, &block) if logger
        end
      METHOD
    end

    # Set color by using a string or one of the defined constants. If a third
    # option is set to +true+, it also adds bold to the string. This is based
    # on the Highline implementation and will automatically append CLEAR to the
    # end of the returned String.
    def color(text, color, bold=false)
      return text unless colorize_logging
      color = self.class.const_get(color.upcase) if color.is_a?(Symbol)
      bold  = bold ? BOLD : ""
      "#{bold}#{color}#{text}#{CLEAR}"
    end
  end
end
require 'active_support/log_subscriber'
require 'active_support/logger'
require 'active_support/notifications'

module ActiveSupport
  class LogSubscriber
    # Provides some helpers to deal with testing log subscribers by setting up
    # notifications. Take for instance Active Record subscriber tests:
    #
    #   class SyncLogSubscriberTest < ActiveSupport::TestCase
    #     include ActiveSupport::LogSubscriber::TestHelper
    #
    #     def setup
    #       ActiveRecord::LogSubscriber.attach_to(:active_record)
    #     end
    #
    #     def test_basic_query_logging
    #       Developer.all.to_a
    #       wait
    #       assert_equal 1, @logger.logged(:debug).size
    #       assert_match(/Developer Load/, @logger.logged(:debug).last)
    #       assert_match(/SELECT \* FROM "developers"/, @logger.logged(:debug).last)
    #     end
    #   end
    #
    # All you need to do is to ensure that your log subscriber is added to
    # Rails::Subscriber, as in the second line of the code above. The test
    # helpers are responsible for setting up the queue, subscriptions and
    # turning colors in logs off.
    #
    # The messages are available in the @logger instance, which is a logger with
    # limited powers (it actually does not send anything to your output), and
    # you can collect them doing @logger.logged(level), where level is the level
    # used in logging, like info, debug, warn and so on.
    module TestHelper
      def setup
        @logger   = MockLogger.new
        @notifier = ActiveSupport::Notifications::Fanout.new

        ActiveSupport::LogSubscriber.colorize_logging = false

        @old_notifier = ActiveSupport::Notifications.notifier
        set_logger(@logger)
        ActiveSupport::Notifications.notifier = @notifier
      end

      def teardown
        set_logger(nil)
        ActiveSupport::Notifications.notifier = @old_notifier
      end

      class MockLogger
        include ActiveSupport::Logger::Severity

        attr_reader :flush_count
        attr_accessor :level

        def initialize(level = DEBUG)
          @flush_count = 0
          @level = level
          @logged = Hash.new { |h,k| h[k] = [] }
        end

        def method_missing(level, message = nil)
           if block_given?
             @logged[level] << yield
           else
             @logged[level] << message
           end
        end

        def logged(level)
          @logged[level].compact.map { |l| l.to_s.strip }
        end

        def flush
          @flush_count += 1
        end

        ActiveSupport::Logger::Severity.constants.each do |severity|
          class_eval <<-EOT, __FILE__, __LINE__ + 1
            def #{severity.downcase}?
              #{severity} >= @level
            end
          EOT
        end
      end

      # Wait notifications to be published.
      def wait
        @notifier.wait
      end

      # Overwrite if you use another logger in your log subscriber.
      #
      #   def logger
      #     ActiveRecord::Base.logger = @logger
      #   end
      def set_logger(logger)
        ActiveSupport::LogSubscriber.logger = logger
      end
    end
  end
end
require 'active_support/core_ext/module/attribute_accessors'
require 'active_support/logger_silence'
require 'logger'

module ActiveSupport
  class Logger < ::Logger
    include LoggerSilence

    # Broadcasts logs to multiple loggers.
    def self.broadcast(logger) # :nodoc:
      Module.new do
        define_method(:add) do |*args, &block|
          logger.add(*args, &block)
          super(*args, &block)
        end

        define_method(:<<) do |x|
          logger << x
          super(x)
        end

        define_method(:close) do
          logger.close
          super()
        end

        define_method(:progname=) do |name|
          logger.progname = name
          super(name)
        end

        define_method(:formatter=) do |formatter|
          logger.formatter = formatter
          super(formatter)
        end

        define_method(:level=) do |level|
          logger.level = level
          super(level)
        end
      end
    end

    def initialize(*args)
      super
      @formatter = SimpleFormatter.new
    end

    # Simple formatter which only displays the message.
    class SimpleFormatter < ::Logger::Formatter
      # This method is invoked when a log event occurs
      def call(severity, timestamp, progname, msg)
        "#{String === msg ? msg : msg.inspect}\n"
      end
    end
  end
end
require 'active_support/concern'

module LoggerSilence
  extend ActiveSupport::Concern
  
  included do
    cattr_accessor :silencer
    self.silencer = true
  end

  # Silences the logger for the duration of the block.
  def silence(temporary_level = Logger::ERROR)
    if silencer
      begin
        old_logger_level, self.level = level, temporary_level
        yield self
      ensure
        self.level = old_logger_level
      end
    else
      yield self
    end
  end
endrequire 'openssl'
require 'base64'
require 'active_support/core_ext/array/extract_options'

module ActiveSupport
  # MessageEncryptor is a simple way to encrypt values which get stored
  # somewhere you don't trust.
  #
  # The cipher text and initialization vector are base64 encoded and returned
  # to you.
  #
  # This can be used in situations similar to the <tt>MessageVerifier</tt>, but
  # where you don't want users to be able to determine the value of the payload.
  #
  #   salt  = SecureRandom.random_bytes(64)
  #   key   = ActiveSupport::KeyGenerator.new('password').generate_key(salt) # => "\x89\xE0\x156\xAC..."
  #   crypt = ActiveSupport::MessageEncryptor.new(key)                       # => #<ActiveSupport::MessageEncryptor ...>
  #   encrypted_data = crypt.encrypt_and_sign('my secret data')              # => "NlFBTTMwOUV5UlA1QlNEN2xkY2d6eThYWWh..."
  #   crypt.decrypt_and_verify(encrypted_data)                               # => "my secret data"
  class MessageEncryptor
    module NullSerializer #:nodoc:
      def self.load(value)
        value
      end

      def self.dump(value)
        value
      end
    end

    class InvalidMessage < StandardError; end
    OpenSSLCipherError = OpenSSL::Cipher::CipherError

    # Initialize a new MessageEncryptor. +secret+ must be at least as long as
    # the cipher key size. For the default 'aes-256-cbc' cipher, this is 256
    # bits. If you are using a user-entered secret, you can generate a suitable
    # key with <tt>OpenSSL::Digest::SHA256.new(user_secret).digest</tt> or
    # similar.
    #
    # Options:
    # * <tt>:cipher</tt>     - Cipher to use. Can be any cipher returned by
    #   <tt>OpenSSL::Cipher.ciphers</tt>. Default is 'aes-256-cbc'.
    # * <tt>:digest</tt> - String of digest to use for signing. Default is +SHA1+.
    # * <tt>:serializer</tt> - Object serializer to use. Default is +Marshal+.
    def initialize(secret, *signature_key_or_options)
      options = signature_key_or_options.extract_options!
      sign_secret = signature_key_or_options.first
      @secret = secret
      @sign_secret = sign_secret
      @cipher = options[:cipher] || 'aes-256-cbc'
      @verifier = MessageVerifier.new(@sign_secret || @secret, digest: options[:digest] || 'SHA1', serializer: NullSerializer)
      @serializer = options[:serializer] || Marshal
    end

    # Encrypt and sign a message. We need to sign the message in order to avoid
    # padding attacks. Reference: http://www.limited-entropy.com/padding-oracle-attacks.
    def encrypt_and_sign(value)
      verifier.generate(_encrypt(value))
    end

    # Decrypt and verify a message. We need to verify the message in order to
    # avoid padding attacks. Reference: http://www.limited-entropy.com/padding-oracle-attacks.
    def decrypt_and_verify(value)
      _decrypt(verifier.verify(value))
    end

    private

    def _encrypt(value)
      cipher = new_cipher
      cipher.encrypt
      cipher.key = @secret

      # Rely on OpenSSL for the initialization vector
      iv = cipher.random_iv

      encrypted_data = cipher.update(@serializer.dump(value))
      encrypted_data << cipher.final

      "#{::Base64.strict_encode64 encrypted_data}--#{::Base64.strict_encode64 iv}"
    end

    def _decrypt(encrypted_message)
      cipher = new_cipher
      encrypted_data, iv = encrypted_message.split("--").map {|v| ::Base64.strict_decode64(v)}

      cipher.decrypt
      cipher.key = @secret
      cipher.iv  = iv

      decrypted_data = cipher.update(encrypted_data)
      decrypted_data << cipher.final

      @serializer.load(decrypted_data)
    rescue OpenSSLCipherError, TypeError, ArgumentError
      raise InvalidMessage
    end

    def new_cipher
      OpenSSL::Cipher::Cipher.new(@cipher)
    end

    def verifier
      @verifier
    end
  end
end
require 'base64'
require 'active_support/core_ext/object/blank'
require 'active_support/security_utils'

module ActiveSupport
  # +MessageVerifier+ makes it easy to generate and verify messages which are
  # signed to prevent tampering.
  #
  # This is useful for cases like remember-me tokens and auto-unsubscribe links
  # where the session store isn't suitable or available.
  #
  # Remember Me:
  #   cookies[:remember_me] = @verifier.generate([@user.id, 2.weeks.from_now])
  #
  # In the authentication filter:
  #
  #   id, time = @verifier.verify(cookies[:remember_me])
  #   if time < Time.now
  #     self.current_user = User.find(id)
  #   end
  #
  # By default it uses Marshal to serialize the message. If you want to use
  # another serialization method, you can set the serializer in the options
  # hash upon initialization:
  #
  #   @verifier = ActiveSupport::MessageVerifier.new('s3Krit', serializer: YAML)
  class MessageVerifier
    class InvalidSignature < StandardError; end

    def initialize(secret, options = {})
      raise ArgumentError, 'Secret should not be nil.' unless secret
      @secret = secret
      @digest = options[:digest] || 'SHA1'
      @serializer = options[:serializer] || Marshal
    end

    # Checks if a signed message could have been generated by signing an object
    # with the +MessageVerifier+'s secret.
    #
    #   verifier = ActiveSupport::MessageVerifier.new 's3Krit'
    #   signed_message = verifier.generate 'a private message'
    #   verifier.valid_message?(signed_message) # => true
    #
    #   tampered_message = signed_message.chop # editing the message invalidates the signature
    #   verifier.valid_message?(tampered_message) # => false
    def valid_message?(signed_message)
      return if signed_message.blank?

      data, digest = signed_message.split("--")
      data.present? && digest.present? && ActiveSupport::SecurityUtils.secure_compare(digest, generate_digest(data))
    end

    # Decodes the signed message using the +MessageVerifier+'s secret.
    #
    #   verifier = ActiveSupport::MessageVerifier.new 's3Krit'
    #
    #   signed_message = verifier.generate 'a private message'
    #   verifier.verified(signed_message) # => 'a private message'
    #
    # Returns +nil+ if the message was not signed with the same secret.
    #
    #   other_verifier = ActiveSupport::MessageVerifier.new 'd1ff3r3nt-s3Krit'
    #   other_verifier.verified(signed_message) # => nil
    #
    # Returns +nil+ if the message is not Base64-encoded.
    #
    #   invalid_message = "f--46a0120593880c733a53b6dad75b42ddc1c8996d"
    #   verifier.verified(invalid_message) # => nil
    #
    # Raises any error raised while decoding the signed message.
    #
    #   incompatible_message = "test--dad7b06c94abba8d46a15fafaef56c327665d5ff"
    #   verifier.verified(incompatible_message) # => TypeError: incompatible marshal file format
    def verified(signed_message)
      if valid_message?(signed_message)
        begin
          data = signed_message.split("--")[0]
          @serializer.load(decode(data))
        rescue ArgumentError => argument_error
          return if argument_error.message =~ %r{invalid base64}
          raise
        end
      end
    end

    # Decodes the signed message using the +MessageVerifier+'s secret.
    #
    #   verifier = ActiveSupport::MessageVerifier.new 's3Krit'
    #   signed_message = verifier.generate 'a private message'
    #
    #   verifier.verify(signed_message) # => 'a private message'
    #
    # Raises +InvalidSignature+ if the message was not signed with the same
    # secret or was not Base64-encoded.
    #
    #   other_verifier = ActiveSupport::MessageVerifier.new 'd1ff3r3nt-s3Krit'
    #   other_verifier.verify(signed_message) # => ActiveSupport::MessageVerifier::InvalidSignature
    def verify(signed_message)
      verified(signed_message) || raise(InvalidSignature)
    end

    # Generates a signed message for the provided value.
    #
    # The message is signed with the +MessageVerifier+'s secret. Without knowing
    # the secret, the original value cannot be extracted from the message.
    #
    #   verifier = ActiveSupport::MessageVerifier.new 's3Krit'
    #   verifier.generate 'a private message' # => "BAhJIhRwcml2YXRlLW1lc3NhZ2UGOgZFVA==--e2d724331ebdee96a10fb99b089508d1c72bd772"
    def generate(value)
      data = encode(@serializer.dump(value))
      "#{data}--#{generate_digest(data)}"
    end

    private
      def encode(data)
        ::Base64.strict_encode64(data)
      end

      def decode(data)
        ::Base64.strict_decode64(data)
      end

      def generate_digest(data)
        require 'openssl' unless defined?(OpenSSL)
        OpenSSL::HMAC.hexdigest(OpenSSL::Digest.const_get(@digest).new, @secret, data)
      end
  end
end
module ActiveSupport #:nodoc:
  module Multibyte
    autoload :Chars, 'active_support/multibyte/chars'
    autoload :Unicode, 'active_support/multibyte/unicode'

    # The proxy class returned when calling mb_chars. You can use this accessor
    # to configure your own proxy class so you can support other encodings. See
    # the ActiveSupport::Multibyte::Chars implementation for an example how to
    # do this.
    #
    #   ActiveSupport::Multibyte.proxy_class = CharsForUTF32
    def self.proxy_class=(klass)
      @proxy_class = klass
    end

    # Returns the current proxy class.
    def self.proxy_class
      @proxy_class ||= ActiveSupport::Multibyte::Chars
    end
  end
end
# encoding: utf-8
require 'active_support/json'
require 'active_support/core_ext/string/access'
require 'active_support/core_ext/string/behavior'
require 'active_support/core_ext/module/delegation'

module ActiveSupport #:nodoc:
  module Multibyte #:nodoc:
    # Chars enables you to work transparently with UTF-8 encoding in the Ruby
    # String class without having extensive knowledge about the encoding. A
    # Chars object accepts a string upon initialization and proxies String
    # methods in an encoding safe manner. All the normal String methods are also
    # implemented on the proxy.
    #
    # String methods are proxied through the Chars object, and can be accessed
    # through the +mb_chars+ method. Methods which would normally return a
    # String object now return a Chars object so methods can be chained.
    #
    #   'The Perfect String  '.mb_chars.downcase.strip.normalize # => "the perfect string"
    #
    # Chars objects are perfectly interchangeable with String objects as long as
    # no explicit class checks are made. If certain methods do explicitly check
    # the class, call +to_s+ before you pass chars objects to them.
    #
    #   bad.explicit_checking_method 'T'.mb_chars.downcase.to_s
    #
    # The default Chars implementation assumes that the encoding of the string
    # is UTF-8, if you want to handle different encodings you can write your own
    # multibyte string handler and configure it through
    # ActiveSupport::Multibyte.proxy_class.
    #
    #   class CharsForUTF32
    #     def size
    #       @wrapped_string.size / 4
    #     end
    #
    #     def self.accepts?(string)
    #       string.length % 4 == 0
    #     end
    #   end
    #
    #   ActiveSupport::Multibyte.proxy_class = CharsForUTF32
    class Chars
      include Comparable
      attr_reader :wrapped_string
      alias to_s wrapped_string
      alias to_str wrapped_string

      delegate :<=>, :=~, :acts_like_string?, :to => :wrapped_string

      # Creates a new Chars instance by wrapping _string_.
      def initialize(string)
        @wrapped_string = string
        @wrapped_string.force_encoding(Encoding::UTF_8) unless @wrapped_string.frozen?
      end

      # Forward all undefined methods to the wrapped string.
      def method_missing(method, *args, &block)
        result = @wrapped_string.__send__(method, *args, &block)
        if method.to_s =~ /!$/
          self if result
        else
          result.kind_of?(String) ? chars(result) : result
        end
      end

      # Returns +true+ if _obj_ responds to the given method. Private methods
      # are included in the search only if the optional second parameter
      # evaluates to +true+.
      def respond_to_missing?(method, include_private)
        @wrapped_string.respond_to?(method, include_private)
      end

      # Returns +true+ when the proxy class can handle the string. Returns
      # +false+ otherwise.
      def self.consumes?(string)
        string.encoding == Encoding::UTF_8
      end

      # Works just like <tt>String#split</tt>, with the exception that the items
      # in the resulting list are Chars instances instead of String. This makes
      # chaining methods easier.
      #
      #   'Café périferôl'.mb_chars.split(/é/).map { |part| part.upcase.to_s } # => ["CAF", " P", "RIFERÔL"]
      def split(*args)
        @wrapped_string.split(*args).map { |i| self.class.new(i) }
      end

      # Works like like <tt>String#slice!</tt>, but returns an instance of
      # Chars, or nil if the string was not modified.
      def slice!(*args)
        chars(@wrapped_string.slice!(*args))
      end

      # Reverses all characters in the string.
      #
      #   'Café'.mb_chars.reverse.to_s # => 'éfaC'
      def reverse
        chars(Unicode.unpack_graphemes(@wrapped_string).reverse.flatten.pack('U*'))
      end

      # Limits the byte size of the string to a number of bytes without breaking
      # characters. Usable when the storage for a string is limited for some
      # reason.
      #
      #   'こんにちは'.mb_chars.limit(7).to_s # => "こん"
      def limit(limit)
        slice(0...translate_offset(limit))
      end

      # Converts characters in the string to uppercase.
      #
      #   'Laurent, où sont les tests ?'.mb_chars.upcase.to_s # => "LAURENT, OÙ SONT LES TESTS ?"
      def upcase
        chars Unicode.upcase(@wrapped_string)
      end

      # Converts characters in the string to lowercase.
      #
      #   'VĚDA A VÝZKUM'.mb_chars.downcase.to_s # => "věda a výzkum"
      def downcase
        chars Unicode.downcase(@wrapped_string)
      end

      # Converts characters in the string to the opposite case.
      #
      #    'El Cañón".mb_chars.swapcase.to_s # => "eL cAÑÓN"
      def swapcase
        chars Unicode.swapcase(@wrapped_string)
      end

      # Converts the first character to uppercase and the remainder to lowercase.
      #
      #  'über'.mb_chars.capitalize.to_s # => "Über"
      def capitalize
        (slice(0) || chars('')).upcase + (slice(1..-1) || chars('')).downcase
      end

      # Capitalizes the first letter of every word, when possible.
      #
      #   "ÉL QUE SE ENTERÓ".mb_chars.titleize    # => "Él Que Se Enteró"
      #   "日本語".mb_chars.titleize                 # => "日本語"
      def titleize
        chars(downcase.to_s.gsub(/\b('?\S)/u) { Unicode.upcase($1)})
      end
      alias_method :titlecase, :titleize

      # Returns the KC normalization of the string by default. NFKC is
      # considered the best normalization form for passing strings to databases
      # and validations.
      #
      # * <tt>form</tt> - The form you want to normalize in. Should be one of the following:
      #   <tt>:c</tt>, <tt>:kc</tt>, <tt>:d</tt>, or <tt>:kd</tt>. Default is
      #   ActiveSupport::Multibyte::Unicode.default_normalization_form
      def normalize(form = nil)
        chars(Unicode.normalize(@wrapped_string, form))
      end

      # Performs canonical decomposition on all the characters.
      #
      #   'é'.length                         # => 2
      #   'é'.mb_chars.decompose.to_s.length # => 3
      def decompose
        chars(Unicode.decompose(:canonical, @wrapped_string.codepoints.to_a).pack('U*'))
      end

      # Performs composition on all the characters.
      #
      #   'é'.length                       # => 3
      #   'é'.mb_chars.compose.to_s.length # => 2
      def compose
        chars(Unicode.compose(@wrapped_string.codepoints.to_a).pack('U*'))
      end

      # Returns the number of grapheme clusters in the string.
      #
      #   'क्षि'.mb_chars.length   # => 4
      #   'क्षि'.mb_chars.grapheme_length # => 3
      def grapheme_length
        Unicode.unpack_graphemes(@wrapped_string).length
      end

      # Replaces all ISO-8859-1 or CP1252 characters by their UTF-8 equivalent
      # resulting in a valid UTF-8 string.
      #
      # Passing +true+ will forcibly tidy all bytes, assuming that the string's
      # encoding is entirely CP1252 or ISO-8859-1.
      def tidy_bytes(force = false)
        chars(Unicode.tidy_bytes(@wrapped_string, force))
      end

      def as_json(options = nil) #:nodoc:
        to_s.as_json(options)
      end

      %w(capitalize downcase reverse tidy_bytes upcase).each do |method|
        define_method("#{method}!") do |*args|
          @wrapped_string = send(method, *args).to_s
          self
        end
      end

      protected

        def translate_offset(byte_offset) #:nodoc:
          return nil if byte_offset.nil?
          return 0   if @wrapped_string == ''

          begin
            @wrapped_string.byteslice(0...byte_offset).unpack('U*').length
          rescue ArgumentError
            byte_offset -= 1
            retry
          end
        end

        def chars(string) #:nodoc:
          self.class.new(string)
        end
    end
  end
end
# encoding: utf-8
module ActiveSupport
  module Multibyte
    module Unicode

      extend self

      # A list of all available normalization forms.
      # See http://www.unicode.org/reports/tr15/tr15-29.html for more
      # information about normalization.
      NORMALIZATION_FORMS = [:c, :kc, :d, :kd]

      # The Unicode version that is supported by the implementation
      UNICODE_VERSION = '7.0.0'

      # The default normalization used for operations that require
      # normalization. It can be set to any of the normalizations
      # in NORMALIZATION_FORMS.
      #
      #   ActiveSupport::Multibyte::Unicode.default_normalization_form = :c
      attr_accessor :default_normalization_form
      @default_normalization_form = :kc

      # Hangul character boundaries and properties
      HANGUL_SBASE = 0xAC00
      HANGUL_LBASE = 0x1100
      HANGUL_VBASE = 0x1161
      HANGUL_TBASE = 0x11A7
      HANGUL_LCOUNT = 19
      HANGUL_VCOUNT = 21
      HANGUL_TCOUNT = 28
      HANGUL_NCOUNT = HANGUL_VCOUNT * HANGUL_TCOUNT
      HANGUL_SCOUNT = 11172
      HANGUL_SLAST = HANGUL_SBASE + HANGUL_SCOUNT
      HANGUL_JAMO_FIRST = 0x1100
      HANGUL_JAMO_LAST = 0x11FF

      # All the unicode whitespace
      WHITESPACE = [
        (0x0009..0x000D).to_a, # White_Space # Cc   [5] <control-0009>..<control-000D>
        0x0020,                # White_Space # Zs       SPACE
        0x0085,                # White_Space # Cc       <control-0085>
        0x00A0,                # White_Space # Zs       NO-BREAK SPACE
        0x1680,                # White_Space # Zs       OGHAM SPACE MARK
        (0x2000..0x200A).to_a, # White_Space # Zs  [11] EN QUAD..HAIR SPACE
        0x2028,                # White_Space # Zl       LINE SEPARATOR
        0x2029,                # White_Space # Zp       PARAGRAPH SEPARATOR
        0x202F,                # White_Space # Zs       NARROW NO-BREAK SPACE
        0x205F,                # White_Space # Zs       MEDIUM MATHEMATICAL SPACE
        0x3000,                # White_Space # Zs       IDEOGRAPHIC SPACE
      ].flatten.freeze

      # BOM (byte order mark) can also be seen as whitespace, it's a
      # non-rendering character used to distinguish between little and big
      # endian. This is not an issue in utf-8, so it must be ignored.
      LEADERS_AND_TRAILERS = WHITESPACE + [65279] # ZERO-WIDTH NO-BREAK SPACE aka BOM

      # Returns a regular expression pattern that matches the passed Unicode
      # codepoints.
      def self.codepoints_to_pattern(array_of_codepoints) #:nodoc:
        array_of_codepoints.collect{ |e| [e].pack 'U*' }.join('|')
      end
      TRAILERS_PAT = /(#{codepoints_to_pattern(LEADERS_AND_TRAILERS)})+\Z/u
      LEADERS_PAT = /\A(#{codepoints_to_pattern(LEADERS_AND_TRAILERS)})+/u

      # Detect whether the codepoint is in a certain character class. Returns
      # +true+ when it's in the specified character class and +false+ otherwise.
      # Valid character classes are: <tt>:cr</tt>, <tt>:lf</tt>, <tt>:l</tt>,
      # <tt>:v</tt>, <tt>:lv</tt>, <tt>:lvt</tt> and <tt>:t</tt>.
      #
      # Primarily used by the grapheme cluster support.
      def in_char_class?(codepoint, classes)
        classes.detect { |c| database.boundary[c] === codepoint } ? true : false
      end

      # Unpack the string at grapheme boundaries. Returns a list of character
      # lists.
      #
      #   Unicode.unpack_graphemes('क्षि') # => [[2325, 2381], [2359], [2367]]
      #   Unicode.unpack_graphemes('Café') # => [[67], [97], [102], [233]]
      def unpack_graphemes(string)
        codepoints = string.codepoints.to_a
        unpacked = []
        pos = 0
        marker = 0
        eoc = codepoints.length
        while(pos < eoc)
          pos += 1
          previous = codepoints[pos-1]
          current = codepoints[pos]
          if (
              # CR X LF
              ( previous == database.boundary[:cr] and current == database.boundary[:lf] ) or
              # L X (L|V|LV|LVT)
              ( database.boundary[:l] === previous and in_char_class?(current, [:l,:v,:lv,:lvt]) ) or
              # (LV|V) X (V|T)
              ( in_char_class?(previous, [:lv,:v]) and in_char_class?(current, [:v,:t]) ) or
              # (LVT|T) X (T)
              ( in_char_class?(previous, [:lvt,:t]) and database.boundary[:t] === current ) or
              # X Extend
              (database.boundary[:extend] === current)
            )
          else
            unpacked << codepoints[marker..pos-1]
            marker = pos
          end
        end
        unpacked
      end

      # Reverse operation of unpack_graphemes.
      #
      #   Unicode.pack_graphemes(Unicode.unpack_graphemes('क्षि')) # => 'क्षि'
      def pack_graphemes(unpacked)
        unpacked.flatten.pack('U*')
      end

      # Re-order codepoints so the string becomes canonical.
      def reorder_characters(codepoints)
        length = codepoints.length- 1
        pos = 0
        while pos < length do
          cp1, cp2 = database.codepoints[codepoints[pos]], database.codepoints[codepoints[pos+1]]
          if (cp1.combining_class > cp2.combining_class) && (cp2.combining_class > 0)
            codepoints[pos..pos+1] = cp2.code, cp1.code
            pos += (pos > 0 ? -1 : 1)
          else
            pos += 1
          end
        end
        codepoints
      end

      # Decompose composed characters to the decomposed form.
      def decompose(type, codepoints)
        codepoints.inject([]) do |decomposed, cp|
          # if it's a hangul syllable starter character
          if HANGUL_SBASE <= cp and cp < HANGUL_SLAST
            sindex = cp - HANGUL_SBASE
            ncp = [] # new codepoints
            ncp << HANGUL_LBASE + sindex / HANGUL_NCOUNT
            ncp << HANGUL_VBASE + (sindex % HANGUL_NCOUNT) / HANGUL_TCOUNT
            tindex = sindex % HANGUL_TCOUNT
            ncp << (HANGUL_TBASE + tindex) unless tindex == 0
            decomposed.concat ncp
          # if the codepoint is decomposable in with the current decomposition type
          elsif (ncp = database.codepoints[cp].decomp_mapping) and (!database.codepoints[cp].decomp_type || type == :compatibility)
            decomposed.concat decompose(type, ncp.dup)
          else
            decomposed << cp
          end
        end
      end

      # Compose decomposed characters to the composed form.
      def compose(codepoints)
        pos = 0
        eoa = codepoints.length - 1
        starter_pos = 0
        starter_char = codepoints[0]
        previous_combining_class = -1
        while pos < eoa
          pos += 1
          lindex = starter_char - HANGUL_LBASE
          # -- Hangul
          if 0 <= lindex and lindex < HANGUL_LCOUNT
            vindex = codepoints[starter_pos+1] - HANGUL_VBASE rescue vindex = -1
            if 0 <= vindex and vindex < HANGUL_VCOUNT
              tindex = codepoints[starter_pos+2] - HANGUL_TBASE rescue tindex = -1
              if 0 <= tindex and tindex < HANGUL_TCOUNT
                j = starter_pos + 2
                eoa -= 2
              else
                tindex = 0
                j = starter_pos + 1
                eoa -= 1
              end
              codepoints[starter_pos..j] = (lindex * HANGUL_VCOUNT + vindex) * HANGUL_TCOUNT + tindex + HANGUL_SBASE
            end
            starter_pos += 1
            starter_char = codepoints[starter_pos]
          # -- Other characters
          else
            current_char = codepoints[pos]
            current = database.codepoints[current_char]
            if current.combining_class > previous_combining_class
              if ref = database.composition_map[starter_char]
                composition = ref[current_char]
              else
                composition = nil
              end
              unless composition.nil?
                codepoints[starter_pos] = composition
                starter_char = composition
                codepoints.delete_at pos
                eoa -= 1
                pos -= 1
                previous_combining_class = -1
              else
                previous_combining_class = current.combining_class
              end
            else
              previous_combining_class = current.combining_class
            end
            if current.combining_class == 0
              starter_pos = pos
              starter_char = codepoints[pos]
            end
          end
        end
        codepoints
      end

      # Rubinius' String#scrub, however, doesn't support ASCII-incompatible chars.
      if !defined?(Rubinius)
        # Replaces all ISO-8859-1 or CP1252 characters by their UTF-8 equivalent
        # resulting in a valid UTF-8 string.
        #
        # Passing +true+ will forcibly tidy all bytes, assuming that the string's
        # encoding is entirely CP1252 or ISO-8859-1.
        def tidy_bytes(string, force = false)
          return string if string.empty?
          return recode_windows1252_chars(string) if force
          string.scrub { |bad| recode_windows1252_chars(bad) }
        end
      else
        def tidy_bytes(string, force = false)
          return string if string.empty?
          return recode_windows1252_chars(string) if force

          # We can't transcode to the same format, so we choose a nearly-identical encoding.
          # We're going to 'transcode' bytes from UTF-8 when possible, then fall back to
          # CP1252 when we get errors. The final string will be 'converted' back to UTF-8
          # before returning.
          reader = Encoding::Converter.new(Encoding::UTF_8, Encoding::UTF_16LE)

          source = string.dup
          out = ''.force_encoding(Encoding::UTF_16LE)

          loop do
            reader.primitive_convert(source, out)
            _, _, _, error_bytes, _ = reader.primitive_errinfo
            break if error_bytes.nil?
            out << error_bytes.encode(Encoding::UTF_16LE, Encoding::Windows_1252, invalid: :replace, undef: :replace)
          end

          reader.finish

          out.encode!(Encoding::UTF_8)
        end
      end

      # Returns the KC normalization of the string by default. NFKC is
      # considered the best normalization form for passing strings to databases
      # and validations.
      #
      # * <tt>string</tt> - The string to perform normalization on.
      # * <tt>form</tt> - The form you want to normalize in. Should be one of
      #   the following: <tt>:c</tt>, <tt>:kc</tt>, <tt>:d</tt>, or <tt>:kd</tt>.
      #   Default is ActiveSupport::Multibyte.default_normalization_form.
      def normalize(string, form=nil)
        form ||= @default_normalization_form
        # See http://www.unicode.org/reports/tr15, Table 1
        codepoints = string.codepoints.to_a
        case form
          when :d
            reorder_characters(decompose(:canonical, codepoints))
          when :c
            compose(reorder_characters(decompose(:canonical, codepoints)))
          when :kd
            reorder_characters(decompose(:compatibility, codepoints))
          when :kc
            compose(reorder_characters(decompose(:compatibility, codepoints)))
          else
            raise ArgumentError, "#{form} is not a valid normalization variant", caller
        end.pack('U*')
      end

      def downcase(string)
        apply_mapping string, :lowercase_mapping
      end

      def upcase(string)
        apply_mapping string, :uppercase_mapping
      end

      def swapcase(string)
        apply_mapping string, :swapcase_mapping
      end

      # Holds data about a codepoint in the Unicode database.
      class Codepoint
        attr_accessor :code, :combining_class, :decomp_type, :decomp_mapping, :uppercase_mapping, :lowercase_mapping

        # Initializing Codepoint object with default values
        def initialize
          @combining_class = 0
          @uppercase_mapping = 0
          @lowercase_mapping = 0
        end

        def swapcase_mapping
          uppercase_mapping > 0 ? uppercase_mapping : lowercase_mapping
        end
      end

      # Holds static data from the Unicode database.
      class UnicodeDatabase
        ATTRIBUTES = :codepoints, :composition_exclusion, :composition_map, :boundary, :cp1252

        attr_writer(*ATTRIBUTES)

        def initialize
          @codepoints = Hash.new(Codepoint.new)
          @composition_exclusion = []
          @composition_map = {}
          @boundary = {}
          @cp1252 = {}
        end

        # Lazy load the Unicode database so it's only loaded when it's actually used
        ATTRIBUTES.each do |attr_name|
          class_eval(<<-EOS, __FILE__, __LINE__ + 1)
            def #{attr_name}     # def codepoints
              load               #   load
              @#{attr_name}      #   @codepoints
            end                  # end
          EOS
        end

        # Loads the Unicode database and returns all the internal objects of
        # UnicodeDatabase.
        def load
          begin
            @codepoints, @composition_exclusion, @composition_map, @boundary, @cp1252 = File.open(self.class.filename, 'rb') { |f| Marshal.load f.read }
          rescue => e
            raise IOError.new("Couldn't load the Unicode tables for UTF8Handler (#{e.message}), ActiveSupport::Multibyte is unusable")
          end

          # Redefine the === method so we can write shorter rules for grapheme cluster breaks
          @boundary.each do |k,_|
            @boundary[k].instance_eval do
              def ===(other)
                detect { |i| i === other } ? true : false
              end
            end if @boundary[k].kind_of?(Array)
          end

          # define attr_reader methods for the instance variables
          class << self
            attr_reader(*ATTRIBUTES)
          end
        end

        # Returns the directory in which the data files are stored.
        def self.dirname
          File.dirname(__FILE__) + '/../values/'
        end

        # Returns the filename for the data file for this version.
        def self.filename
          File.expand_path File.join(dirname, "unicode_tables.dat")
        end
      end

      private

      def apply_mapping(string, mapping) #:nodoc:
        database.codepoints
        string.each_codepoint.map do |codepoint|
          cp = database.codepoints[codepoint]
          if cp and (ncp = cp.send(mapping)) and ncp > 0
            ncp
          else
            codepoint
          end
        end.pack('U*')
      end

      def recode_windows1252_chars(string)
        string.encode(Encoding::UTF_8, Encoding::Windows_1252, invalid: :replace, undef: :replace)
      end

      def database
        @database ||= UnicodeDatabase.new
      end
    end
  end
end
require 'active_support/notifications/instrumenter'
require 'active_support/notifications/fanout'
require 'active_support/per_thread_registry'

module ActiveSupport
  # = Notifications
  #
  # <tt>ActiveSupport::Notifications</tt> provides an instrumentation API for
  # Ruby.
  #
  # == Instrumenters
  #
  # To instrument an event you just need to do:
  #
  #   ActiveSupport::Notifications.instrument('render', extra: :information) do
  #     render text: 'Foo'
  #   end
  #
  # That first executes the block and then notifies all subscribers once done.
  #
  # In the example above +render+ is the name of the event, and the rest is called
  # the _payload_. The payload is a mechanism that allows instrumenters to pass
  # extra information to subscribers. Payloads consist of a hash whose contents
  # are arbitrary and generally depend on the event.
  #
  # == Subscribers
  #
  # You can consume those events and the information they provide by registering
  # a subscriber.
  #
  #   ActiveSupport::Notifications.subscribe('render') do |name, start, finish, id, payload|
  #     name    # => String, name of the event (such as 'render' from above)
  #     start   # => Time, when the instrumented block started execution
  #     finish  # => Time, when the instrumented block ended execution
  #     id      # => String, unique ID for this notification
  #     payload # => Hash, the payload
  #   end
  #
  # For instance, let's store all "render" events in an array:
  #
  #   events = []
  #
  #   ActiveSupport::Notifications.subscribe('render') do |*args|
  #     events << ActiveSupport::Notifications::Event.new(*args)
  #   end
  #
  # That code returns right away, you are just subscribing to "render" events.
  # The block is saved and will be called whenever someone instruments "render":
  #
  #   ActiveSupport::Notifications.instrument('render', extra: :information) do
  #     render text: 'Foo'
  #   end
  #
  #   event = events.first
  #   event.name      # => "render"
  #   event.duration  # => 10 (in milliseconds)
  #   event.payload   # => { extra: :information }
  #
  # The block in the <tt>subscribe</tt> call gets the name of the event, start
  # timestamp, end timestamp, a string with a unique identifier for that event
  # (something like "535801666f04d0298cd6"), and a hash with the payload, in
  # that order.
  #
  # If an exception happens during that particular instrumentation the payload will
  # have a key <tt>:exception</tt> with an array of two elements as value: a string with
  # the name of the exception class, and the exception message.
  #
  # As the previous example depicts, the class <tt>ActiveSupport::Notifications::Event</tt>
  # is able to take the arguments as they come and provide an object-oriented
  # interface to that data.
  #
  # It is also possible to pass an object as the second parameter passed to the
  # <tt>subscribe</tt> method instead of a block:
  #
  #   module ActionController
  #     class PageRequest
  #       def call(name, started, finished, unique_id, payload)
  #         Rails.logger.debug ['notification:', name, started, finished, unique_id, payload].join(' ')
  #       end
  #     end
  #   end
  #
  #   ActiveSupport::Notifications.subscribe('process_action.action_controller', ActionController::PageRequest.new)
  #
  # resulting in the following output within the logs including a hash with the payload:
  #
  #   notification: process_action.action_controller 2012-04-13 01:08:35 +0300 2012-04-13 01:08:35 +0300 af358ed7fab884532ec7 {
  #      controller: "Devise::SessionsController",
  #      action: "new",
  #      params: {"action"=>"new", "controller"=>"devise/sessions"},
  #      format: :html,
  #      method: "GET",
  #      path: "/login/sign_in",
  #      status: 200,
  #      view_runtime: 279.3080806732178,
  #      db_runtime: 40.053
  #    }
  #
  # You can also subscribe to all events whose name matches a certain regexp:
  #
  #   ActiveSupport::Notifications.subscribe(/render/) do |*args|
  #     ...
  #   end
  #
  # and even pass no argument to <tt>subscribe</tt>, in which case you are subscribing
  # to all events.
  #
  # == Temporary Subscriptions
  #
  # Sometimes you do not want to subscribe to an event for the entire life of
  # the application. There are two ways to unsubscribe.
  #
  # WARNING: The instrumentation framework is designed for long-running subscribers,
  # use this feature sparingly because it wipes some internal caches and that has
  # a negative impact on performance.
  #
  # === Subscribe While a Block Runs
  #
  # You can subscribe to some event temporarily while some block runs. For
  # example, in
  #
  #   callback = lambda {|*args| ... }
  #   ActiveSupport::Notifications.subscribed(callback, "sql.active_record") do
  #     ...
  #   end
  #
  # the callback will be called for all "sql.active_record" events instrumented
  # during the execution of the block. The callback is unsubscribed automatically
  # after that.
  #
  # === Manual Unsubscription
  #
  # The +subscribe+ method returns a subscriber object:
  #
  #   subscriber = ActiveSupport::Notifications.subscribe("render") do |*args|
  #     ...
  #   end
  #
  # To prevent that block from being called anymore, just unsubscribe passing
  # that reference:
  #
  #   ActiveSupport::Notifications.unsubscribe(subscriber)
  #
  # You can also unsubscribe by passing the name of the subscriber object. Note
  # that this will unsubscribe all subscriptions with the given name:
  #
  #   ActiveSupport::Notifications.unsubscribe("render")
  #
  # == Default Queue
  #
  # Notifications ships with a queue implementation that consumes and publishes events
  # to all log subscribers. You can use any queue implementation you want.
  #
  module Notifications
    class << self
      attr_accessor :notifier

      def publish(name, *args)
        notifier.publish(name, *args)
      end

      def instrument(name, payload = {})
        if notifier.listening?(name)
          instrumenter.instrument(name, payload) { yield payload if block_given? }
        else
          yield payload if block_given?
        end
      end

      def subscribe(*args, &block)
        notifier.subscribe(*args, &block)
      end

      def subscribed(callback, *args, &block)
        subscriber = subscribe(*args, &callback)
        yield
      ensure
        unsubscribe(subscriber)
      end

      def unsubscribe(subscriber_or_name)
        notifier.unsubscribe(subscriber_or_name)
      end

      def instrumenter
        InstrumentationRegistry.instance.instrumenter_for(notifier)
      end
    end

    # This class is a registry which holds all of the +Instrumenter+ objects
    # in a particular thread local. To access the +Instrumenter+ object for a
    # particular +notifier+, you can call the following method:
    #
    #   InstrumentationRegistry.instrumenter_for(notifier)
    #
    # The instrumenters for multiple notifiers are held in a single instance of
    # this class.
    class InstrumentationRegistry # :nodoc:
      extend ActiveSupport::PerThreadRegistry

      def initialize
        @registry = {}
      end

      def instrumenter_for(notifier)
        @registry[notifier] ||= Instrumenter.new(notifier)
      end
    end

    self.notifier = Fanout.new
  end
end
require 'mutex_m'
require 'thread_safe'

module ActiveSupport
  module Notifications
    # This is a default queue implementation that ships with Notifications.
    # It just pushes events to all registered log subscribers.
    #
    # This class is thread safe. All methods are reentrant.
    class Fanout
      include Mutex_m

      def initialize
        @subscribers = []
        @listeners_for = ThreadSafe::Cache.new
        super
      end

      def subscribe(pattern = nil, block = Proc.new)
        subscriber = Subscribers.new pattern, block
        synchronize do
          @subscribers << subscriber
          @listeners_for.clear
        end
        subscriber
      end

      def unsubscribe(subscriber_or_name)
        synchronize do
          case subscriber_or_name
          when String
            @subscribers.reject! { |s| s.matches?(subscriber_or_name) }
          else
            @subscribers.delete(subscriber_or_name)
          end

          @listeners_for.clear
        end
      end

      def start(name, id, payload)
        listeners_for(name).each { |s| s.start(name, id, payload) }
      end

      def finish(name, id, payload)
        listeners_for(name).each { |s| s.finish(name, id, payload) }
      end

      def publish(name, *args)
        listeners_for(name).each { |s| s.publish(name, *args) }
      end

      def listeners_for(name)
        # this is correctly done double-checked locking (ThreadSafe::Cache's lookups have volatile semantics)
        @listeners_for[name] || synchronize do
          # use synchronisation when accessing @subscribers
          @listeners_for[name] ||= @subscribers.select { |s| s.subscribed_to?(name) }
        end
      end

      def listening?(name)
        listeners_for(name).any?
      end

      # This is a sync queue, so there is no waiting.
      def wait
      end

      module Subscribers # :nodoc:
        def self.new(pattern, listener)
          if listener.respond_to?(:start) and listener.respond_to?(:finish)
            subscriber = Evented.new pattern, listener
          else
            subscriber = Timed.new pattern, listener
          end

          unless pattern
            AllMessages.new(subscriber)
          else
            subscriber
          end
        end

        class Evented #:nodoc:
          def initialize(pattern, delegate)
            @pattern = pattern
            @delegate = delegate
            @can_publish = delegate.respond_to?(:publish)
          end

          def publish(name, *args)
            if @can_publish
              @delegate.publish name, *args
            end
          end

          def start(name, id, payload)
            @delegate.start name, id, payload
          end

          def finish(name, id, payload)
            @delegate.finish name, id, payload
          end

          def subscribed_to?(name)
            @pattern === name
          end

          def matches?(name)
            @pattern && @pattern === name
          end
        end

        class Timed < Evented
          def publish(name, *args)
            @delegate.call name, *args
          end

          def start(name, id, payload)
            timestack = Thread.current[:_timestack] ||= []
            timestack.push Time.now
          end

          def finish(name, id, payload)
            timestack = Thread.current[:_timestack]
            started = timestack.pop
            @delegate.call(name, started, Time.now, id, payload)
          end
        end

        class AllMessages # :nodoc:
          def initialize(delegate)
            @delegate = delegate
          end

          def start(name, id, payload)
            @delegate.start name, id, payload
          end

          def finish(name, id, payload)
            @delegate.finish name, id, payload
          end

          def publish(name, *args)
            @delegate.publish name, *args
          end

          def subscribed_to?(name)
            true
          end

          alias :matches? :===
        end
      end
    end
  end
end
require 'securerandom'

module ActiveSupport
  module Notifications
    # Instrumenters are stored in a thread local.
    class Instrumenter
      attr_reader :id

      def initialize(notifier)
        @id       = unique_id
        @notifier = notifier
      end

      # Instrument the given block by measuring the time taken to execute it
      # and publish it. Notice that events get sent even if an error occurs
      # in the passed-in block.
      def instrument(name, payload={})
        start name, payload
        begin
          yield payload
        rescue Exception => e
          payload[:exception] = [e.class.name, e.message]
          raise e
        ensure
          finish name, payload
        end
      end

      # Send a start notification with +name+ and +payload+.
      def start(name, payload)
        @notifier.start name, @id, payload
      end

      # Send a finish notification with +name+ and +payload+.
      def finish(name, payload)
        @notifier.finish name, @id, payload
      end

      private

      def unique_id
        SecureRandom.hex(10)
      end
    end

    class Event
      attr_reader :name, :time, :transaction_id, :payload, :children
      attr_accessor :end

      def initialize(name, start, ending, transaction_id, payload)
        @name           = name
        @payload        = payload.dup
        @time           = start
        @transaction_id = transaction_id
        @end            = ending
        @children       = []
        @duration       = nil
      end

      # Returns the difference in milliseconds between when the execution of the
      # event started and when it ended.
      #
      #   ActiveSupport::Notifications.subscribe('wait') do |*args|
      #     @event = ActiveSupport::Notifications::Event.new(*args)
      #   end
      #
      #   ActiveSupport::Notifications.instrument('wait') do
      #     sleep 1
      #   end
      #
      #   @event.duration # => 1000.138
      def duration
        @duration ||= 1000.0 * (self.end - time)
      end

      def <<(event)
        @children << event
      end

      def parent_of?(event)
        @children.include? event
      end
    end
  end
end
module ActiveSupport
  module NumberHelper
    extend ActiveSupport::Autoload

    eager_autoload do
      autoload :NumberConverter
      autoload :NumberToRoundedConverter
      autoload :NumberToDelimitedConverter
      autoload :NumberToHumanConverter
      autoload :NumberToHumanSizeConverter
      autoload :NumberToPhoneConverter
      autoload :NumberToCurrencyConverter
      autoload :NumberToPercentageConverter
    end

    extend self

    # Formats a +number+ into a US phone number (e.g., (555)
    # 123-9876). You can customize the format in the +options+ hash.
    #
    # ==== Options
    #
    # * <tt>:area_code</tt> - Adds parentheses around the area code.
    # * <tt>:delimiter</tt> - Specifies the delimiter to use
    #   (defaults to "-").
    # * <tt>:extension</tt> - Specifies an extension to add to the
    #   end of the generated number.
    # * <tt>:country_code</tt> - Sets the country code for the phone
    #   number.
    # ==== Examples
    #
    #   number_to_phone(5551234)                                     # => 555-1234
    #   number_to_phone('5551234')                                   # => 555-1234
    #   number_to_phone(1235551234)                                  # => 123-555-1234
    #   number_to_phone(1235551234, area_code: true)                 # => (123) 555-1234
    #   number_to_phone(1235551234, delimiter: ' ')                  # => 123 555 1234
    #   number_to_phone(1235551234, area_code: true, extension: 555) # => (123) 555-1234 x 555
    #   number_to_phone(1235551234, country_code: 1)                 # => +1-123-555-1234
    #   number_to_phone('123a456')                                   # => 123a456
    #
    #   number_to_phone(1235551234, country_code: 1, extension: 1343, delimiter: '.')
    #   # => +1.123.555.1234 x 1343
    def number_to_phone(number, options = {})
      NumberToPhoneConverter.convert(number, options)
    end

    # Formats a +number+ into a currency string (e.g., $13.65). You
    # can customize the format in the +options+ hash.
    #
    # ==== Options
    #
    # * <tt>:locale</tt> - Sets the locale to be used for formatting
    #   (defaults to current locale).
    # * <tt>:precision</tt> - Sets the level of precision (defaults
    #   to 2).
    # * <tt>:unit</tt> - Sets the denomination of the currency
    #   (defaults to "$").
    # * <tt>:separator</tt> - Sets the separator between the units
    #   (defaults to ".").
    # * <tt>:delimiter</tt> - Sets the thousands delimiter (defaults
    #   to ",").
    # * <tt>:format</tt> - Sets the format for non-negative numbers
    #   (defaults to "%u%n").  Fields are <tt>%u</tt> for the
    #   currency, and <tt>%n</tt> for the number.
    # * <tt>:negative_format</tt> - Sets the format for negative
    #   numbers (defaults to prepending an hyphen to the formatted
    #   number given by <tt>:format</tt>).  Accepts the same fields
    #   than <tt>:format</tt>, except <tt>%n</tt> is here the
    #   absolute value of the number.
    #
    # ==== Examples
    #
    #   number_to_currency(1234567890.50)                # => $1,234,567,890.50
    #   number_to_currency(1234567890.506)               # => $1,234,567,890.51
    #   number_to_currency(1234567890.506, precision: 3) # => $1,234,567,890.506
    #   number_to_currency(1234567890.506, locale: :fr)  # => 1 234 567 890,51 €
    #   number_to_currency('123a456')                    # => $123a456
    #
    #   number_to_currency(-1234567890.50, negative_format: '(%u%n)')
    #   # => ($1,234,567,890.50)
    #   number_to_currency(1234567890.50, unit: '&pound;', separator: ',', delimiter: '')
    #   # => &pound;1234567890,50
    #   number_to_currency(1234567890.50, unit: '&pound;', separator: ',', delimiter: '', format: '%n %u')
    #   # => 1234567890,50 &pound;
    def number_to_currency(number, options = {})
      NumberToCurrencyConverter.convert(number, options)
    end

    # Formats a +number+ as a percentage string (e.g., 65%). You can
    # customize the format in the +options+ hash.
    #
    # ==== Options
    #
    # * <tt>:locale</tt> - Sets the locale to be used for formatting
    #   (defaults to current locale).
    # * <tt>:precision</tt> - Sets the precision of the number
    #   (defaults to 3). Keeps the number's precision if nil.
    # * <tt>:significant</tt> - If +true+, precision will be the number
    #   of significant_digits. If +false+, the number of fractional
    #   digits (defaults to +false+).
    # * <tt>:separator</tt> - Sets the separator between the
    #   fractional and integer digits (defaults to ".").
    # * <tt>:delimiter</tt> - Sets the thousands delimiter (defaults
    #   to "").
    # * <tt>:strip_insignificant_zeros</tt> - If +true+ removes
    #   insignificant zeros after the decimal separator (defaults to
    #   +false+).
    # * <tt>:format</tt> - Specifies the format of the percentage
    #   string The number field is <tt>%n</tt> (defaults to "%n%").
    #
    # ==== Examples
    #
    #   number_to_percentage(100)                                  # => 100.000%
    #   number_to_percentage('98')                                 # => 98.000%
    #   number_to_percentage(100, precision: 0)                    # => 100%
    #   number_to_percentage(1000, delimiter: '.', separator: ',') # => 1.000,000%
    #   number_to_percentage(302.24398923423, precision: 5)        # => 302.24399%
    #   number_to_percentage(1000, locale: :fr)                    # => 1 000,000%
    #   number_to_percentage:(1000, precision: nil)                # => 1000%
    #   number_to_percentage('98a')                                # => 98a%
    #   number_to_percentage(100, format: '%n  %')                 # => 100  %
    def number_to_percentage(number, options = {})
      NumberToPercentageConverter.convert(number, options)
    end

    # Formats a +number+ with grouped thousands using +delimiter+
    # (e.g., 12,324). You can customize the format in the +options+
    # hash.
    #
    # ==== Options
    #
    # * <tt>:locale</tt> - Sets the locale to be used for formatting
    #   (defaults to current locale).
    # * <tt>:delimiter</tt> - Sets the thousands delimiter (defaults
    #   to ",").
    # * <tt>:separator</tt> - Sets the separator between the
    #   fractional and integer digits (defaults to ".").
    #
    # ==== Examples
    #
    #   number_to_delimited(12345678)                    # => 12,345,678
    #   number_to_delimited('123456')                    # => 123,456
    #   number_to_delimited(12345678.05)                 # => 12,345,678.05
    #   number_to_delimited(12345678, delimiter: '.')    # => 12.345.678
    #   number_to_delimited(12345678, delimiter: ',')    # => 12,345,678
    #   number_to_delimited(12345678.05, separator: ' ') # => 12,345,678 05
    #   number_to_delimited(12345678.05, locale: :fr)    # => 12 345 678,05
    #   number_to_delimited('112a')                      # => 112a
    #   number_to_delimited(98765432.98, delimiter: ' ', separator: ',')
    #   # => 98 765 432,98
    def number_to_delimited(number, options = {})
      NumberToDelimitedConverter.convert(number, options)
    end

    # Formats a +number+ with the specified level of
    # <tt>:precision</tt> (e.g., 112.32 has a precision of 2 if
    # +:significant+ is +false+, and 5 if +:significant+ is +true+).
    # You can customize the format in the +options+ hash.
    #
    # ==== Options
    #
    # * <tt>:locale</tt> - Sets the locale to be used for formatting
    #   (defaults to current locale).
    # * <tt>:precision</tt> - Sets the precision of the number
    #   (defaults to 3). Keeps the number's precision if nil.
    # * <tt>:significant</tt> - If +true+, precision will be the number
    #   of significant_digits. If +false+, the number of fractional
    #   digits (defaults to +false+).
    # * <tt>:separator</tt> - Sets the separator between the
    #   fractional and integer digits (defaults to ".").
    # * <tt>:delimiter</tt> - Sets the thousands delimiter (defaults
    #   to "").
    # * <tt>:strip_insignificant_zeros</tt> - If +true+ removes
    #   insignificant zeros after the decimal separator (defaults to
    #   +false+).
    #
    # ==== Examples
    #
    #   number_to_rounded(111.2345)                                  # => 111.235
    #   number_to_rounded(111.2345, precision: 2)                    # => 111.23
    #   number_to_rounded(13, precision: 5)                          # => 13.00000
    #   number_to_rounded(389.32314, precision: 0)                   # => 389
    #   number_to_rounded(111.2345, significant: true)               # => 111
    #   number_to_rounded(111.2345, precision: 1, significant: true) # => 100
    #   number_to_rounded(13, precision: 5, significant: true)       # => 13.000
    #   number_to_rounded(13, precision: nil)                        # => 13
    #   number_to_rounded(111.234, locale: :fr)                      # => 111,234
    #
    #   number_to_rounded(13, precision: 5, significant: true, strip_insignificant_zeros: true)
    #   # => 13
    #
    #   number_to_rounded(389.32314, precision: 4, significant: true) # => 389.3
    #   number_to_rounded(1111.2345, precision: 2, separator: ',', delimiter: '.')
    #   # => 1.111,23
    def number_to_rounded(number, options = {})
      NumberToRoundedConverter.convert(number, options)
    end

    # Formats the bytes in +number+ into a more understandable
    # representation (e.g., giving it 1500 yields 1.5 KB). This
    # method is useful for reporting file sizes to users. You can
    # customize the format in the +options+ hash.
    #
    # See <tt>number_to_human</tt> if you want to pretty-print a
    # generic number.
    #
    # ==== Options
    #
    # * <tt>:locale</tt> - Sets the locale to be used for formatting
    #   (defaults to current locale).
    # * <tt>:precision</tt> - Sets the precision of the number
    #   (defaults to 3).
    # * <tt>:significant</tt> - If +true+, precision will be the number
    #   of significant_digits. If +false+, the number of fractional
    #   digits (defaults to +true+)
    # * <tt>:separator</tt> - Sets the separator between the
    #   fractional and integer digits (defaults to ".").
    # * <tt>:delimiter</tt> - Sets the thousands delimiter (defaults
    #   to "").
    # * <tt>:strip_insignificant_zeros</tt> - If +true+ removes
    #   insignificant zeros after the decimal separator (defaults to
    #   +true+)
    # * <tt>:prefix</tt> - If +:si+ formats the number using the SI
    #   prefix (defaults to :binary)
    #
    # ==== Examples
    #
    #   number_to_human_size(123)                                    # => 123 Bytes
    #   number_to_human_size(1234)                                   # => 1.21 KB
    #   number_to_human_size(12345)                                  # => 12.1 KB
    #   number_to_human_size(1234567)                                # => 1.18 MB
    #   number_to_human_size(1234567890)                             # => 1.15 GB
    #   number_to_human_size(1234567890123)                          # => 1.12 TB
    #   number_to_human_size(1234567, precision: 2)                  # => 1.2 MB
    #   number_to_human_size(483989, precision: 2)                   # => 470 KB
    #   number_to_human_size(1234567, precision: 2, separator: ',')  # => 1,2 MB
    #   number_to_human_size(1234567890123, precision: 5)            # => "1.1228 TB"
    #   number_to_human_size(524288000, precision: 5)                # => "500 MB"
    def number_to_human_size(number, options = {})
      NumberToHumanSizeConverter.convert(number, options)
    end

    # Pretty prints (formats and approximates) a number in a way it
    # is more readable by humans (eg.: 1200000000 becomes "1.2
    # Billion"). This is useful for numbers that can get very large
    # (and too hard to read).
    #
    # See <tt>number_to_human_size</tt> if you want to print a file
    # size.
    #
    # You can also define your own unit-quantifier names if you want
    # to use other decimal units (eg.: 1500 becomes "1.5
    # kilometers", 0.150 becomes "150 milliliters", etc). You may
    # define a wide range of unit quantifiers, even fractional ones
    # (centi, deci, mili, etc).
    #
    # ==== Options
    #
    # * <tt>:locale</tt> - Sets the locale to be used for formatting
    #   (defaults to current locale).
    # * <tt>:precision</tt> - Sets the precision of the number
    #   (defaults to 3).
    # * <tt>:significant</tt> - If +true+, precision will be the number
    #   of significant_digits. If +false+, the number of fractional
    #   digits (defaults to +true+)
    # * <tt>:separator</tt> - Sets the separator between the
    #   fractional and integer digits (defaults to ".").
    # * <tt>:delimiter</tt> - Sets the thousands delimiter (defaults
    #   to "").
    # * <tt>:strip_insignificant_zeros</tt> - If +true+ removes
    #   insignificant zeros after the decimal separator (defaults to
    #   +true+)
    # * <tt>:units</tt> - A Hash of unit quantifier names. Or a
    #   string containing an i18n scope where to find this hash. It
    #   might have the following keys:
    #   * *integers*: <tt>:unit</tt>, <tt>:ten</tt>,
    #     <tt>:hundred</tt>, <tt>:thousand</tt>, <tt>:million</tt>,
    #     <tt>:billion</tt>, <tt>:trillion</tt>,
    #     <tt>:quadrillion</tt>
    #   * *fractionals*: <tt>:deci</tt>, <tt>:centi</tt>,
    #     <tt>:mili</tt>, <tt>:micro</tt>, <tt>:nano</tt>,
    #     <tt>:pico</tt>, <tt>:femto</tt>
    # * <tt>:format</tt> - Sets the format of the output string
    #   (defaults to "%n %u"). The field types are:
    #   * %u - The quantifier (ex.: 'thousand')
    #   * %n - The number
    #
    # ==== Examples
    #
    #   number_to_human(123)                         # => "123"
    #   number_to_human(1234)                        # => "1.23 Thousand"
    #   number_to_human(12345)                       # => "12.3 Thousand"
    #   number_to_human(1234567)                     # => "1.23 Million"
    #   number_to_human(1234567890)                  # => "1.23 Billion"
    #   number_to_human(1234567890123)               # => "1.23 Trillion"
    #   number_to_human(1234567890123456)            # => "1.23 Quadrillion"
    #   number_to_human(1234567890123456789)         # => "1230 Quadrillion"
    #   number_to_human(489939, precision: 2)        # => "490 Thousand"
    #   number_to_human(489939, precision: 4)        # => "489.9 Thousand"
    #   number_to_human(1234567, precision: 4,
    #                            significant: false) # => "1.2346 Million"
    #   number_to_human(1234567, precision: 1,
    #                            separator: ',',
    #                            significant: false) # => "1,2 Million"
    #
    #   number_to_human(500000000, precision: 5)           # => "500 Million"
    #   number_to_human(12345012345, significant: false)   # => "12.345 Billion"
    #
    # Non-significant zeros after the decimal separator are stripped
    # out by default (set <tt>:strip_insignificant_zeros</tt> to
    # +false+ to change that):
    #
    # number_to_human(12.00001)                                       # => "12"
    # number_to_human(12.00001, strip_insignificant_zeros: false)     # => "12.0"
    #
    # ==== Custom Unit Quantifiers
    #
    # You can also use your own custom unit quantifiers:
    #  number_to_human(500000, units: { unit: 'ml', thousand: 'lt' })  # => "500 lt"
    #
    # If in your I18n locale you have:
    #
    #   distance:
    #     centi:
    #       one: "centimeter"
    #       other: "centimeters"
    #     unit:
    #       one: "meter"
    #       other: "meters"
    #     thousand:
    #       one: "kilometer"
    #       other: "kilometers"
    #     billion: "gazillion-distance"
    #
    # Then you could do:
    #
    #   number_to_human(543934, units: :distance)            # => "544 kilometers"
    #   number_to_human(54393498, units: :distance)          # => "54400 kilometers"
    #   number_to_human(54393498000, units: :distance)       # => "54.4 gazillion-distance"
    #   number_to_human(343, units: :distance, precision: 1) # => "300 meters"
    #   number_to_human(1, units: :distance)                 # => "1 meter"
    #   number_to_human(0.34, units: :distance)              # => "34 centimeters"
    def number_to_human(number, options = {})
      NumberToHumanConverter.convert(number, options)
    end
  end
end
require 'active_support/core_ext/big_decimal/conversions'
require 'active_support/core_ext/object/blank'
require 'active_support/core_ext/hash/keys'
require 'active_support/i18n'
require 'active_support/core_ext/class/attribute'

module ActiveSupport
  module NumberHelper
    class NumberConverter # :nodoc:
      # Default and i18n option namespace per class
      class_attribute :namespace

      # Does the object need a number that is a valid float?
      class_attribute :validate_float

      attr_reader :number, :opts

      DEFAULTS = {
        # Used in number_to_delimited
        # These are also the defaults for 'currency', 'percentage', 'precision', and 'human'
        format: {
          # Sets the separator between the units, for more precision (e.g. 1.0 / 2.0 == 0.5)
          separator: ".",
          # Delimits thousands (e.g. 1,000,000 is a million) (always in groups of three)
          delimiter: ",",
          # Number of decimals, behind the separator (the number 1 with a precision of 2 gives: 1.00)
          precision: 3,
          # If set to true, precision will mean the number of significant digits instead
          # of the number of decimal digits (1234 with precision 2 becomes 1200, 1.23543 becomes 1.2)
          significant: false,
          # If set, the zeros after the decimal separator will always be stripped (eg.: 1.200 will be 1.2)
          strip_insignificant_zeros: false
        },

        # Used in number_to_currency
        currency: {
          format: {
            format: "%u%n",
            negative_format: "-%u%n",
            unit: "$",
            # These five are to override number.format and are optional
            separator: ".",
            delimiter: ",",
            precision: 2,
            significant: false,
            strip_insignificant_zeros: false
          }
        },

        # Used in number_to_percentage
        percentage: {
          format: {
            delimiter: "",
            format: "%n%"
          }
        },

        # Used in number_to_rounded
        precision: {
          format: {
            delimiter: ""
          }
        },

        # Used in number_to_human_size and number_to_human
        human: {
          format: {
            # These five are to override number.format and are optional
            delimiter: "",
            precision: 3,
            significant: true,
            strip_insignificant_zeros: true
          },
          # Used in number_to_human_size
          storage_units: {
            # Storage units output formatting.
            # %u is the storage unit, %n is the number (default: 2 MB)
            format: "%n %u",
            units: {
              byte: "Bytes",
              kb: "KB",
              mb: "MB",
              gb: "GB",
              tb: "TB"
            }
          },
          # Used in number_to_human
          decimal_units: {
            format: "%n %u",
            # Decimal units output formatting
            # By default we will only quantify some of the exponents
            # but the commented ones might be defined or overridden
            # by the user.
            units: {
              # femto: Quadrillionth
              # pico: Trillionth
              # nano: Billionth
              # micro: Millionth
              # mili: Thousandth
              # centi: Hundredth
              # deci: Tenth
              unit: "",
              # ten:
              #   one: Ten
              #   other: Tens
              # hundred: Hundred
              thousand: "Thousand",
              million: "Million",
              billion: "Billion",
              trillion: "Trillion",
              quadrillion: "Quadrillion"
            }
          }
        }
      }

      def self.convert(number, options)
        new(number, options).execute
      end

      def initialize(number, options)
        @number = number
        @opts   = options.symbolize_keys
      end

      def execute
        if !number
          nil
        elsif validate_float? && !valid_float?
          number
        else
          convert
        end
      end

      private

        def options
          @options ||= format_options.merge(opts)
        end

        def format_options #:nodoc:
          default_format_options.merge!(i18n_format_options)
        end

        def default_format_options #:nodoc:
          options = DEFAULTS[:format].dup
          options.merge!(DEFAULTS[namespace][:format]) if namespace
          options
        end

        def i18n_format_options #:nodoc:
          locale = opts[:locale]
          options = I18n.translate(:'number.format', locale: locale, default: {}).dup

          if namespace
            options.merge!(I18n.translate(:"number.#{namespace}.format", locale: locale, default: {}))
          end

          options
        end

        def translate_number_value_with_default(key, i18n_options = {}) #:nodoc:
          I18n.translate(key, { default: default_value(key), scope: :number }.merge!(i18n_options))
        end

        def translate_in_locale(key, i18n_options = {})
          translate_number_value_with_default(key, { locale: options[:locale] }.merge(i18n_options))
        end

        def default_value(key)
          key.split('.').reduce(DEFAULTS) { |defaults, k| defaults[k.to_sym] }
        end

        def valid_float? #:nodoc:
          Float(number)
        rescue ArgumentError, TypeError
          false
        end
    end
  end
end
module ActiveSupport
  module NumberHelper
    class NumberToCurrencyConverter < NumberConverter # :nodoc:
      self.namespace = :currency

      def convert
        number = self.number.to_s.strip
        format = options[:format]

        if is_negative?(number)
          format = options[:negative_format]
          number = absolute_value(number)
        end

        rounded_number = NumberToRoundedConverter.convert(number, options)
        format.gsub('%n'.freeze, rounded_number).gsub('%u'.freeze, options[:unit])
      end

      private

        def is_negative?(number)
          number.to_f.phase != 0
        end

        def absolute_value(number)
          number.respond_to?(:abs) ? number.abs : number.sub(/\A-/, '')
        end

        def options
          @options ||= begin
            defaults = default_format_options.merge(i18n_opts)
            # Override negative format if format options are given
            defaults[:negative_format] = "-#{opts[:format]}" if opts[:format]
            defaults.merge!(opts)
          end
        end

        def i18n_opts
          # Set International negative format if it does not exist
          i18n = i18n_format_options
          i18n[:negative_format] ||= "-#{i18n[:format]}" if i18n[:format]
          i18n
        end
    end
  end
end
module ActiveSupport
  module NumberHelper
    class NumberToDelimitedConverter < NumberConverter #:nodoc:
      self.validate_float = true

      DELIMITED_REGEX = /(\d)(?=(\d\d\d)+(?!\d))/

      def convert
        parts.join(options[:separator])
      end

      private

        def parts
          left, right = number.to_s.split('.')
          left.gsub!(DELIMITED_REGEX) do |digit_to_delimit|
            "#{digit_to_delimit}#{options[:delimiter]}"
          end
          [left, right].compact
        end
    end
  end
end
module ActiveSupport
  module NumberHelper
    class NumberToHumanConverter < NumberConverter # :nodoc:
      DECIMAL_UNITS = { 0 => :unit, 1 => :ten, 2 => :hundred, 3 => :thousand, 6 => :million, 9 => :billion, 12 => :trillion, 15 => :quadrillion,
        -1 => :deci, -2 => :centi, -3 => :mili, -6 => :micro, -9 => :nano, -12 => :pico, -15 => :femto }
      INVERTED_DECIMAL_UNITS = DECIMAL_UNITS.invert

      self.namespace      = :human
      self.validate_float = true

      def convert # :nodoc:
        @number = Float(number)

        # for backwards compatibility with those that didn't add strip_insignificant_zeros to their locale files
        unless options.key?(:strip_insignificant_zeros)
          options[:strip_insignificant_zeros] = true
        end

        units = opts[:units]
        exponent = calculate_exponent(units)
        @number = number / (10 ** exponent)

        unit = determine_unit(units, exponent)

        rounded_number = NumberToRoundedConverter.convert(number, options)
        format.gsub('%n'.freeze, rounded_number).gsub('%u'.freeze, unit).strip
      end

      private

        def format
          options[:format] || translate_in_locale('human.decimal_units.format')
        end

        def determine_unit(units, exponent)
          exp = DECIMAL_UNITS[exponent]
          case units
          when Hash
            units[exp] || ''
          when String, Symbol
            I18n.translate("#{units}.#{exp}", :locale => options[:locale], :count => number.to_i)
          else
            translate_in_locale("human.decimal_units.units.#{exp}", count: number.to_i)
          end
        end

        def calculate_exponent(units)
          exponent = number != 0 ? Math.log10(number.abs).floor : 0
          unit_exponents(units).find { |e| exponent >= e } || 0
        end

        def unit_exponents(units)
          case units
          when Hash
            units
          when String, Symbol
            I18n.translate(units.to_s, :locale => options[:locale], :raise => true)
          when nil
            translate_in_locale("human.decimal_units.units", raise: true)
          else
            raise ArgumentError, ":units must be a Hash or String translation scope."
          end.keys.map { |e_name| INVERTED_DECIMAL_UNITS[e_name] }.sort_by(&:-@)
        end
    end
  end
end
module ActiveSupport
  module NumberHelper
    class NumberToHumanSizeConverter < NumberConverter #:nodoc:
      STORAGE_UNITS = [:byte, :kb, :mb, :gb, :tb]

      self.namespace      = :human
      self.validate_float = true

      def convert
        @number = Float(number)

        # for backwards compatibility with those that didn't add strip_insignificant_zeros to their locale files
        unless options.key?(:strip_insignificant_zeros)
          options[:strip_insignificant_zeros] = true
        end

        if smaller_than_base?
          number_to_format = number.to_i.to_s
        else
          human_size = number / (base ** exponent)
          number_to_format = NumberToRoundedConverter.convert(human_size, options)
        end
        conversion_format.gsub('%n'.freeze, number_to_format).gsub('%u'.freeze, unit)
      end

      private

        def conversion_format
          translate_number_value_with_default('human.storage_units.format', :locale => options[:locale], :raise => true)
        end

        def unit
          translate_number_value_with_default(storage_unit_key, :locale => options[:locale], :count => number.to_i, :raise => true)
        end

        def storage_unit_key
          key_end = smaller_than_base? ? 'byte' : STORAGE_UNITS[exponent]
          "human.storage_units.units.#{key_end}"
        end

        def exponent
          max = STORAGE_UNITS.size - 1
          exp = (Math.log(number) / Math.log(base)).to_i
          exp = max if exp > max # avoid overflow for the highest unit
          exp
        end

        def smaller_than_base?
          number.to_i < base
        end

        def base
          opts[:prefix] == :si ? 1000 : 1024
        end
    end
  end
end

module ActiveSupport
  module NumberHelper
    class NumberToPercentageConverter < NumberConverter # :nodoc:
      self.namespace = :percentage

      def convert
        rounded_number = NumberToRoundedConverter.convert(number, options)
        options[:format].gsub('%n'.freeze, rounded_number)
      end
    end
  end
end
module ActiveSupport
  module NumberHelper
    class NumberToPhoneConverter < NumberConverter #:nodoc:
      def convert
        str  = country_code(opts[:country_code])
        str << convert_to_phone_number(number.to_s.strip)
        str << phone_ext(opts[:extension])
      end

      private

        def convert_to_phone_number(number)
          if opts[:area_code]
            convert_with_area_code(number)
          else
            convert_without_area_code(number)
          end
        end

        def convert_with_area_code(number)
          number.gsub!(/(\d{1,3})(\d{3})(\d{4}$)/,"(\\1) \\2#{delimiter}\\3")
          number
        end

        def convert_without_area_code(number)
          number.gsub!(/(\d{0,3})(\d{3})(\d{4})$/,"\\1#{delimiter}\\2#{delimiter}\\3")
          number.slice!(0, 1) if start_with_delimiter?(number)
          number
        end

        def start_with_delimiter?(number)
          delimiter.present? && number.start_with?(delimiter)
        end

        def delimiter
          opts[:delimiter] || "-"
        end

        def country_code(code)
          code.blank? ? "" : "+#{code}#{delimiter}"
        end

        def phone_ext(ext)
          ext.blank? ? "" : " x #{ext}"
        end
    end
  end
end

module ActiveSupport
  module NumberHelper
    class NumberToRoundedConverter < NumberConverter # :nodoc:
      self.namespace      = :precision
      self.validate_float = true

      def convert
        precision = options.delete :precision

        if precision
          case number
          when Float, String
            @number = BigDecimal(number.to_s)
          when Rational
            @number = BigDecimal(number, digit_count(number.to_i) + precision)
          else
            @number = number.to_d
          end

          if options.delete(:significant) && precision > 0
            digits, rounded_number = digits_and_rounded_number(precision)
            precision -= digits
            precision = 0 if precision < 0 # don't let it be negative
          else
            rounded_number = number.round(precision)
            rounded_number = rounded_number.to_i if precision == 0 && rounded_number.finite?
            rounded_number = rounded_number.abs if rounded_number.zero? # prevent showing negative zeros
          end

          formatted_string =
            if BigDecimal === rounded_number && rounded_number.finite?
              s = rounded_number.to_s('F') + '0'*precision
              a, b = s.split('.', 2)
              a + '.' + b[0, precision]
            else
              "%00.#{precision}f" % rounded_number
            end
        else
          formatted_string = number
        end

        delimited_number = NumberToDelimitedConverter.convert(formatted_string, options)
        format_number(delimited_number)
      end

      private

        def digits_and_rounded_number(precision)
          if zero?
            [1, 0]
          else
            digits = digit_count(number)
            multiplier = 10 ** (digits - precision)
            rounded_number = calculate_rounded_number(multiplier)
            digits = digit_count(rounded_number) # After rounding, the number of digits may have changed
            [digits, rounded_number]
          end
        end

        def calculate_rounded_number(multiplier)
          (number / BigDecimal.new(multiplier.to_f.to_s)).round * multiplier
        end

        def digit_count(number)
          number.zero? ? 1 : (Math.log10(absolute_number(number)) + 1).floor
        end

        def strip_insignificant_zeros
          options[:strip_insignificant_zeros]
        end

        def format_number(number)
          if strip_insignificant_zeros
            escaped_separator = Regexp.escape(options[:separator])
            number.sub(/(#{escaped_separator})(\d*[1-9])?0+\z/, '\1\2').sub(/#{escaped_separator}\z/, '')
          else
            number
          end
        end

        def absolute_number(number)
          number.respond_to?(:abs) ? number.abs : number.to_d.abs
        end

        def zero?
          number.respond_to?(:zero?) ? number.zero? : number.to_d.zero?
        end
    end
  end
end
require 'active_support/core_ext/hash/deep_merge'

module ActiveSupport
  class OptionMerger #:nodoc:
    instance_methods.each do |method|
      undef_method(method) if method !~ /^(__|instance_eval|class|object_id)/
    end

    def initialize(context, options)
      @context, @options = context, options
    end

    private
      def method_missing(method, *arguments, &block)
        if arguments.first.is_a?(Proc)
          proc = arguments.pop
          arguments << lambda { |*args| @options.deep_merge(proc.call(*args)) }
        else
          arguments << (arguments.last.respond_to?(:to_hash) ? @options.deep_merge(arguments.pop) : @options.dup)
        end

        @context.__send__(method, *arguments, &block)
      end
  end
end
require 'yaml'

YAML.add_builtin_type("omap") do |type, val|
  ActiveSupport::OrderedHash[val.map{ |v| v.to_a.first }]
end

module ActiveSupport
  # <tt>ActiveSupport::OrderedHash</tt> implements a hash that preserves
  # insertion order.
  #
  #   oh = ActiveSupport::OrderedHash.new
  #   oh[:a] = 1
  #   oh[:b] = 2
  #   oh.keys # => [:a, :b], this order is guaranteed
  #
  # Also, maps the +omap+ feature for YAML files
  # (See http://yaml.org/type/omap.html) to support ordered items
  # when loading from yaml.
  #
  # <tt>ActiveSupport::OrderedHash</tt> is namespaced to prevent conflicts
  # with other implementations.
  class OrderedHash < ::Hash
    def to_yaml_type
      "!tag:yaml.org,2002:omap"
    end

    def encode_with(coder)
      coder.represent_seq '!omap', map { |k,v| { k => v } }
    end

    def select(*args, &block)
      dup.tap { |hash| hash.select!(*args, &block) }
    end

    def reject(*args, &block)
      dup.tap { |hash| hash.reject!(*args, &block) }
    end

    def nested_under_indifferent_access
      self
    end

    # Returns true to make sure that this hash is extractable via <tt>Array#extract_options!</tt>
    def extractable_options?
      true
    end
  end
end
module ActiveSupport
  # Usually key value pairs are handled something like this:
  #
  #   h = {}
  #   h[:boy] = 'John'
  #   h[:girl] = 'Mary'
  #   h[:boy]  # => 'John'
  #   h[:girl] # => 'Mary'
  #
  # Using +OrderedOptions+, the above code could be reduced to:
  #
  #   h = ActiveSupport::OrderedOptions.new
  #   h.boy = 'John'
  #   h.girl = 'Mary'
  #   h.boy  # => 'John'
  #   h.girl # => 'Mary'
  class OrderedOptions < Hash
    alias_method :_get, :[] # preserve the original #[] method
    protected :_get # make it protected

    def []=(key, value)
      super(key.to_sym, value)
    end

    def [](key)
      super(key.to_sym)
    end

    def method_missing(name, *args)
      name_string = name.to_s
      if name_string.chomp!('=')
        self[name_string] = args.first
      else
        self[name]
      end
    end

    def respond_to_missing?(name, include_private)
      true
    end
  end

  # +InheritableOptions+ provides a constructor to build an +OrderedOptions+
  # hash inherited from another hash.
  #
  # Use this if you already have some hash and you want to create a new one based on it.
  #
  #   h = ActiveSupport::InheritableOptions.new({ girl: 'Mary', boy: 'John' })
  #   h.girl # => 'Mary'
  #   h.boy  # => 'John'
  class InheritableOptions < OrderedOptions
    def initialize(parent = nil)
      if parent.kind_of?(OrderedOptions)
        # use the faster _get when dealing with OrderedOptions
        super() { |h,k| parent._get(k) }
      elsif parent
        super() { |h,k| parent[k] }
      else
        super()
      end
    end

    def inheritable_copy
      self.class.new(self)
    end
  end
end
module ActiveSupport
  # This module is used to encapsulate access to thread local variables.
  #
  # Instead of polluting the thread locals namespace:
  #
  #   Thread.current[:connection_handler]
  #
  # you define a class that extends this module:
  #
  #   module ActiveRecord
  #     class RuntimeRegistry
  #       extend ActiveSupport::PerThreadRegistry
  #
  #       attr_accessor :connection_handler
  #     end
  #   end
  #
  # and invoke the declared instance accessors as class methods. So
  #
  #   ActiveRecord::RuntimeRegistry.connection_handler = connection_handler
  #
  # sets a connection handler local to the current thread, and
  #
  #   ActiveRecord::RuntimeRegistry.connection_handler
  #
  # returns a connection handler local to the current thread.
  #
  # This feature is accomplished by instantiating the class and storing the
  # instance as a thread local keyed by the class name. In the example above
  # a key "ActiveRecord::RuntimeRegistry" is stored in <tt>Thread.current</tt>.
  # The class methods proxy to said thread local instance.
  #
  # If the class has an initializer, it must accept no arguments.
  module PerThreadRegistry
    def self.extended(object)
      object.instance_variable_set '@per_thread_registry_key', object.name.freeze
    end

    def instance
      Thread.current[@per_thread_registry_key] ||= new
    end

    protected
      def method_missing(name, *args, &block) # :nodoc:
        # Caches the method definition as a singleton method of the receiver.
        define_singleton_method(name) do |*a, &b|
          instance.public_send(name, *a, &b)
        end

        send(name, *args, &block)
      end
  end
end
module ActiveSupport
  # A class with no predefined methods that behaves similarly to Builder's
  # BlankSlate. Used for proxy classes.
  class ProxyObject < ::BasicObject
    undef_method :==
    undef_method :equal?

    # Let ActiveSupport::ProxyObject at least raise exceptions.
    def raise(*args)
      ::Object.send(:raise, *args)
    end
  end
end
# This is private interface.
#
# Rails components cherry pick from Active Support as needed, but there are a
# few features that are used for sure in some way or another and it is not worth
# putting individual requires absolutely everywhere. Think blank? for example.
#
# This file is loaded by every Rails component except Active Support itself,
# but it does not belong to the Rails public interface. It is internal to
# Rails and can change anytime.

# Defines Object#blank? and Object#present?.
require 'active_support/core_ext/object/blank'

# Rails own autoload, eager_load, etc.
require 'active_support/dependencies/autoload'

# Support for ClassMethods and the included macro.
require 'active_support/concern'

# Defines Class#class_attribute.
require 'active_support/core_ext/class/attribute'

# Defines Module#delegate.
require 'active_support/core_ext/module/delegation'

# Defines ActiveSupport::Deprecation.
require 'active_support/deprecation'
require "active_support"
require "active_support/i18n_railtie"

module ActiveSupport
  class Railtie < Rails::Railtie # :nodoc:
    config.active_support = ActiveSupport::OrderedOptions.new

    config.eager_load_namespaces << ActiveSupport

    initializer "active_support.deprecation_behavior" do |app|
      if deprecation = app.config.active_support.deprecation
        ActiveSupport::Deprecation.behavior = deprecation
      end
    end

    # Sets the default value for Time.zone
    # If assigned value cannot be matched to a TimeZone, an exception will be raised.
    initializer "active_support.initialize_time_zone" do |app|
      begin
        TZInfo::DataSource.get
      rescue TZInfo::DataSourceNotFound => e
        raise e.exception "tzinfo-data is not present. Please add gem 'tzinfo-data' to your Gemfile and run bundle install"
      end
      require 'active_support/core_ext/time/zones'
      zone_default = Time.find_zone!(app.config.time_zone)

      unless zone_default
        raise 'Value assigned to config.time_zone not recognized. ' \
          'Run "rake -D time" for a list of tasks for finding appropriate time zone names.'
      end

      Time.zone_default = zone_default
    end

    # Sets the default week start
    # If assigned value is not a valid day symbol (e.g. :sunday, :monday, ...), an exception will be raised.
    initializer "active_support.initialize_beginning_of_week" do |app|
      require 'active_support/core_ext/date/calculations'
      beginning_of_week_default = Date.find_beginning_of_week!(app.config.beginning_of_week)

      Date.beginning_of_week_default = beginning_of_week_default
    end

    initializer "active_support.set_configs" do |app|
      app.config.active_support.each do |k, v|
        k = "#{k}="
        ActiveSupport.send(k, v) if ActiveSupport.respond_to? k
      end
    end
  end
end
require 'active_support/concern'
require 'active_support/core_ext/class/attribute'
require 'active_support/core_ext/string/inflections'
require 'active_support/core_ext/array/extract_options'

module ActiveSupport
  # Rescuable module adds support for easier exception handling.
  module Rescuable
    extend Concern

    included do
      class_attribute :rescue_handlers
      self.rescue_handlers = []
    end

    module ClassMethods
      # Rescue exceptions raised in controller actions.
      #
      # <tt>rescue_from</tt> receives a series of exception classes or class
      # names, and a trailing <tt>:with</tt> option with the name of a method
      # or a Proc object to be called to handle them. Alternatively a block can
      # be given.
      #
      # Handlers that take one argument will be called with the exception, so
      # that the exception can be inspected when dealing with it.
      #
      # Handlers are inherited. They are searched from right to left, from
      # bottom to top, and up the hierarchy. The handler of the first class for
      # which <tt>exception.is_a?(klass)</tt> holds true is the one invoked, if
      # any.
      #
      #   class ApplicationController < ActionController::Base
      #     rescue_from User::NotAuthorized, with: :deny_access # self defined exception
      #     rescue_from ActiveRecord::RecordInvalid, with: :show_errors
      #
      #     rescue_from 'MyAppError::Base' do |exception|
      #       render xml: exception, status: 500
      #     end
      #
      #     protected
      #       def deny_access
      #         ...
      #       end
      #
      #       def show_errors(exception)
      #         exception.record.new_record? ? ...
      #       end
      #   end
      #
      # Exceptions raised inside exception handlers are not propagated up.
      def rescue_from(*klasses, &block)
        options = klasses.extract_options!

        unless options.has_key?(:with)
          if block_given?
            options[:with] = block
          else
            raise ArgumentError, "Need a handler. Supply an options hash that has a :with key as the last argument."
          end
        end

        klasses.each do |klass|
          key = if klass.is_a?(Module) && klass.respond_to?(:===)
            klass.name
          elsif klass.is_a?(String)
            klass
          else
            raise ArgumentError, "#{klass} is neither an Exception nor a String"
          end

          # Put the new handler at the end because the list is read in reverse.
          self.rescue_handlers += [[key, options[:with]]]
        end
      end
    end

    # Tries to rescue the exception by looking up and calling a registered handler.
    def rescue_with_handler(exception)
      if handler = handler_for_rescue(exception)
        handler.arity != 0 ? handler.call(exception) : handler.call
        true # don't rely on the return value of the handler
      end
    end

    def handler_for_rescue(exception)
      # We go from right to left because pairs are pushed onto rescue_handlers
      # as rescue_from declarations are found.
      _, rescuer = self.class.rescue_handlers.reverse.detect do |klass_name, handler|
        # The purpose of allowing strings in rescue_from is to support the
        # declaration of handler associations for exception classes whose
        # definition is yet unknown.
        #
        # Since this loop needs the constants it would be inconsistent to
        # assume they should exist at this point. An early raised exception
        # could trigger some other handler and the array could include
        # precisely a string whose corresponding constant has not yet been
        # seen. This is why we are tolerant to unknown constants.
        #
        # Note that this tolerance only matters if the exception was given as
        # a string, otherwise a NameError will be raised by the interpreter
        # itself when rescue_from CONSTANT is executed.
        klass = self.class.const_get(klass_name) rescue nil
        klass ||= (klass_name.constantize rescue nil)
        klass === exception if klass
      end

      case rescuer
      when Symbol
        method(rescuer)
      when Proc
        if rescuer.arity == 0
          Proc.new { instance_exec(&rescuer) }
        else
          Proc.new { |_exception| instance_exec(_exception, &rescuer) }
        end
      end
    end
  end
end
module ActiveSupport
  module SecurityUtils
    # Constant time string comparison.
    #
    # The values compared should be of fixed length, such as strings
    # that have already been processed by HMAC. This should not be used
    # on variable length plaintext strings because it could leak length info
    # via timing attacks.
    def secure_compare(a, b)
      return false unless a.bytesize == b.bytesize

      l = a.unpack "C#{a.bytesize}"

      res = 0
      b.each_byte { |byte| res |= byte ^ l.shift }
      res == 0
    end
    module_function :secure_compare
  end
end
module ActiveSupport
  # Wrapping a string in this class gives you a prettier way to test
  # for equality. The value returned by <tt>Rails.env</tt> is wrapped
  # in a StringInquirer object, so instead of calling this:
  #
  #   Rails.env == 'production'
  #
  # you can call this:
  #
  #   Rails.env.production?
  class StringInquirer < String
    private

      def respond_to_missing?(method_name, include_private = false)
        method_name[-1] == '?'
      end

      def method_missing(method_name, *arguments)
        if method_name[-1] == '?'
          self == method_name[0..-2]
        else
          super
        end
      end
  end
end
require 'active_support/per_thread_registry'

module ActiveSupport
  # ActiveSupport::Subscriber is an object set to consume
  # ActiveSupport::Notifications. The subscriber dispatches notifications to
  # a registered object based on its given namespace.
  #
  # An example would be an Active Record subscriber responsible for collecting
  # statistics about queries:
  #
  #   module ActiveRecord
  #     class StatsSubscriber < ActiveSupport::Subscriber
  #       attach_to :active_record
  #
  #       def sql(event)
  #         Statsd.timing("sql.#{event.payload[:name]}", event.duration)
  #       end
  #     end
  #   end
  #
  # After configured, whenever a "sql.active_record" notification is published,
  # it will properly dispatch the event (ActiveSupport::Notifications::Event) to
  # the +sql+ method.
  class Subscriber
    class << self

      # Attach the subscriber to a namespace.
      def attach_to(namespace, subscriber=new, notifier=ActiveSupport::Notifications)
        @namespace  = namespace
        @subscriber = subscriber
        @notifier   = notifier

        subscribers << subscriber

        # Add event subscribers for all existing methods on the class.
        subscriber.public_methods(false).each do |event|
          add_event_subscriber(event)
        end
      end

      # Adds event subscribers for all new methods added to the class.
      def method_added(event)
        # Only public methods are added as subscribers, and only if a notifier
        # has been set up. This means that subscribers will only be set up for
        # classes that call #attach_to.
        if public_method_defined?(event) && notifier
          add_event_subscriber(event)
        end
      end

      def subscribers
        @@subscribers ||= []
      end

      protected

      attr_reader :subscriber, :notifier, :namespace

      def add_event_subscriber(event)
        return if %w{ start finish }.include?(event.to_s)

        pattern = "#{event}.#{namespace}"

        # Don't add multiple subscribers (eg. if methods are redefined).
        return if subscriber.patterns.include?(pattern)

        subscriber.patterns << pattern
        notifier.subscribe(pattern, subscriber)
      end
    end

    attr_reader :patterns # :nodoc:

    def initialize
      @queue_key = [self.class.name, object_id].join "-"
      @patterns  = []
      super
    end

    def start(name, id, payload)
      e = ActiveSupport::Notifications::Event.new(name, Time.now, nil, id, payload)
      parent = event_stack.last
      parent << e if parent

      event_stack.push e
    end

    def finish(name, id, payload)
      finished  = Time.now
      event     = event_stack.pop
      event.end = finished
      event.payload.merge!(payload)

      method = name.split('.'.freeze).first
      send(method, event)
    end

    private

      def event_stack
        SubscriberQueueRegistry.instance.get_queue(@queue_key)
      end
  end

  # This is a registry for all the event stacks kept for subscribers.
  #
  # See the documentation of <tt>ActiveSupport::PerThreadRegistry</tt>
  # for further details.
  class SubscriberQueueRegistry # :nodoc:
    extend PerThreadRegistry

    def initialize
      @registry = {}
    end

    def get_queue(queue_key)
      @registry[queue_key] ||= []
    end
  end
end
require 'active_support/core_ext/module/delegation'
require 'active_support/core_ext/object/blank'
require 'logger'
require 'active_support/logger'

module ActiveSupport
  # Wraps any standard Logger object to provide tagging capabilities.
  #
  #   logger = ActiveSupport::TaggedLogging.new(Logger.new(STDOUT))
  #   logger.tagged('BCX') { logger.info 'Stuff' }                            # Logs "[BCX] Stuff"
  #   logger.tagged('BCX', "Jason") { logger.info 'Stuff' }                   # Logs "[BCX] [Jason] Stuff"
  #   logger.tagged('BCX') { logger.tagged('Jason') { logger.info 'Stuff' } } # Logs "[BCX] [Jason] Stuff"
  #
  # This is used by the default Rails.logger as configured by Railties to make
  # it easy to stamp log lines with subdomains, request ids, and anything else
  # to aid debugging of multi-user production applications.
  module TaggedLogging
    module Formatter # :nodoc:
      # This method is invoked when a log event occurs.
      def call(severity, timestamp, progname, msg)
        super(severity, timestamp, progname, "#{tags_text}#{msg}")
      end

      def tagged(*tags)
        new_tags = push_tags(*tags)
        yield self
      ensure
        pop_tags(new_tags.size)
      end

      def push_tags(*tags)
        tags.flatten.reject(&:blank?).tap do |new_tags|
          current_tags.concat new_tags
        end
      end

      def pop_tags(size = 1)
        current_tags.pop size
      end

      def clear_tags!
        current_tags.clear
      end

      def current_tags
        # We use our object ID here to avoid conflicting with other instances
        thread_key = @thread_key ||= "activesupport_tagged_logging_tags:#{object_id}".freeze
        Thread.current[thread_key] ||= []
      end

      private
        def tags_text
          tags = current_tags
          if tags.any?
            tags.collect { |tag| "[#{tag}] " }.join
          end
        end
    end

    def self.new(logger)
      # Ensure we set a default formatter so we aren't extending nil!
      logger.formatter ||= ActiveSupport::Logger::SimpleFormatter.new
      logger.formatter.extend Formatter
      logger.extend(self)
    end

    delegate :push_tags, :pop_tags, :clear_tags!, to: :formatter

    def tagged(*tags)
      formatter.tagged(*tags) { yield self }
    end

    def flush
      clear_tags!
      super if defined?(super)
    end
  end
end
gem 'minitest' # make sure we get the gem, not stdlib
require 'minitest'
require 'active_support/testing/tagged_logging'
require 'active_support/testing/setup_and_teardown'
require 'active_support/testing/assertions'
require 'active_support/testing/deprecation'
require 'active_support/testing/declarative'
require 'active_support/testing/isolation'
require 'active_support/testing/constant_lookup'
require 'active_support/testing/time_helpers'
require 'active_support/testing/file_fixtures'
require 'active_support/core_ext/kernel/reporting'

module ActiveSupport
  class TestCase < ::Minitest::Test
    Assertion = Minitest::Assertion

    class << self
      # Sets the order in which test cases are run.
      #
      #   ActiveSupport::TestCase.test_order = :random # => :random
      #
      # Valid values are:
      # * +:random+   (to run tests in random order)
      # * +:parallel+ (to run tests in parallel)
      # * +:sorted+   (to run tests alphabetically by method name)
      # * +:alpha+    (equivalent to +:sorted+)
      def test_order=(new_order)
        ActiveSupport.test_order = new_order
      end

      # Returns the order in which test cases are run.
      #
      #   ActiveSupport::TestCase.test_order # => :random
      #
      # Possible values are +:random+, +:parallel+, +:alpha+, +:sorted+.
      # Defaults to +:random+.
      def test_order
        ActiveSupport.test_order ||= :random
      end
    end

    alias_method :method_name, :name

    include ActiveSupport::Testing::TaggedLogging
    include ActiveSupport::Testing::SetupAndTeardown
    include ActiveSupport::Testing::Assertions
    include ActiveSupport::Testing::Deprecation
    include ActiveSupport::Testing::TimeHelpers
    include ActiveSupport::Testing::FileFixtures
    extend ActiveSupport::Testing::Declarative

    # test/unit backwards compatibility methods
    alias :assert_raise :assert_raises
    alias :assert_not_empty :refute_empty
    alias :assert_not_equal :refute_equal
    alias :assert_not_in_delta :refute_in_delta
    alias :assert_not_in_epsilon :refute_in_epsilon
    alias :assert_not_includes :refute_includes
    alias :assert_not_instance_of :refute_instance_of
    alias :assert_not_kind_of :refute_kind_of
    alias :assert_no_match :refute_match
    alias :assert_not_nil :refute_nil
    alias :assert_not_operator :refute_operator
    alias :assert_not_predicate :refute_predicate
    alias :assert_not_respond_to :refute_respond_to
    alias :assert_not_same :refute_same

    # Reveals the intention that the block should not raise any exception.
    #
    #   assert_nothing_raised do
    #     ...
    #   end
    def assert_nothing_raised(*args)
      yield
    end
  end
end
require 'active_support/core_ext/object/blank'

module ActiveSupport
  module Testing
    module Assertions
      # Assert that an expression is not truthy. Passes if <tt>object</tt> is
      # +nil+ or +false+. "Truthy" means "considered true in a conditional"
      # like <tt>if foo</tt>.
      #
      #   assert_not nil    # => true
      #   assert_not false  # => true
      #   assert_not 'foo'  # => Expected "foo" to be nil or false
      #
      # An error message can be specified.
      #
      #   assert_not foo, 'foo should be false'
      def assert_not(object, message = nil)
        message ||= "Expected #{mu_pp(object)} to be nil or false"
        assert !object, message
      end

      # Test numeric difference between the return value of an expression as a
      # result of what is evaluated in the yielded block.
      #
      #   assert_difference 'Article.count' do
      #     post :create, article: {...}
      #   end
      #
      # An arbitrary expression is passed in and evaluated.
      #
      #   assert_difference 'assigns(:article).comments(:reload).size' do
      #     post :create, comment: {...}
      #   end
      #
      # An arbitrary positive or negative difference can be specified.
      # The default is <tt>1</tt>.
      #
      #   assert_difference 'Article.count', -1 do
      #     post :delete, id: ...
      #   end
      #
      # An array of expressions can also be passed in and evaluated.
      #
      #   assert_difference [ 'Article.count', 'Post.count' ], 2 do
      #     post :create, article: {...}
      #   end
      #
      # A lambda or a list of lambdas can be passed in and evaluated:
      #
      #   assert_difference ->{ Article.count }, 2 do
      #     post :create, article: {...}
      #   end
      #
      #   assert_difference [->{ Article.count }, ->{ Post.count }], 2 do
      #     post :create, article: {...}
      #   end
      #
      # An error message can be specified.
      #
      #   assert_difference 'Article.count', -1, 'An Article should be destroyed' do
      #     post :delete, id: ...
      #   end
      def assert_difference(expression, difference = 1, message = nil, &block)
        expressions = Array(expression)

        exps = expressions.map { |e|
          e.respond_to?(:call) ? e : lambda { eval(e, block.binding) }
        }
        before = exps.map(&:call)

        yield

        expressions.zip(exps).each_with_index do |(code, e), i|
          error  = "#{code.inspect} didn't change by #{difference}"
          error  = "#{message}.\n#{error}" if message
          assert_equal(before[i] + difference, e.call, error)
        end
      end

      # Assertion that the numeric result of evaluating an expression is not
      # changed before and after invoking the passed in block.
      #
      #   assert_no_difference 'Article.count' do
      #     post :create, article: invalid_attributes
      #   end
      #
      # An error message can be specified.
      #
      #   assert_no_difference 'Article.count', 'An Article should not be created' do
      #     post :create, article: invalid_attributes
      #   end
      def assert_no_difference(expression, message = nil, &block)
        assert_difference expression, 0, message, &block
      end
    end
  end
end
gem 'minitest'

require 'minitest'

Minitest.autorun
require "active_support/concern"
require "active_support/inflector"

module ActiveSupport
  module Testing
    # Resolves a constant from a minitest spec name.
    #
    # Given the following spec-style test:
    #
    #   describe WidgetsController, :index do
    #     describe "authenticated user" do
    #       describe "returns widgets" do
    #         it "has a controller that exists" do
    #           assert_kind_of WidgetsController, @controller
    #         end
    #       end
    #     end
    #   end
    #
    # The test will have the following name:
    #
    #   "WidgetsController::index::authenticated user::returns widgets"
    #
    # The constant WidgetsController can be resolved from the name.
    # The following code will resolve the constant:
    #
    #   controller = determine_constant_from_test_name(name) do |constant|
    #     Class === constant && constant < ::ActionController::Metal
    #   end
    module ConstantLookup
      extend ::ActiveSupport::Concern

      module ClassMethods  # :nodoc:
        def determine_constant_from_test_name(test_name)
          names = test_name.split "::"
          while names.size > 0 do
            names.last.sub!(/Test$/, "")
            begin
              constant = names.join("::").safe_constantize
              break(constant) if yield(constant)
            ensure
              names.pop
            end
          end
        end
      end

    end
  end
end
module ActiveSupport
  module Testing
    module Declarative
      unless defined?(Spec)
        # Helper to define a test method using a String. Under the hood, it replaces
        # spaces with underscores and defines the test method.
        #
        #   test "verify something" do
        #     ...
        #   end
        def test(name, &block)
          test_name = "test_#{name.gsub(/\s+/,'_')}".to_sym
          defined = method_defined? test_name
          raise "#{test_name} is already defined in #{self}" if defined
          if block_given?
            define_method(test_name, &block)
          else
            define_method(test_name) do
              flunk "No implementation provided for #{name}"
            end
          end
        end
      end
    end
  end
end
require 'active_support/deprecation'

module ActiveSupport
  module Testing
    module Deprecation #:nodoc:
      def assert_deprecated(match = nil, &block)
        result, warnings = collect_deprecations(&block)
        assert !warnings.empty?, "Expected a deprecation warning within the block but received none"
        if match
          match = Regexp.new(Regexp.escape(match)) unless match.is_a?(Regexp)
          assert warnings.any? { |w| w =~ match }, "No deprecation warning matched #{match}: #{warnings.join(', ')}"
        end
        result
      end

      def assert_not_deprecated(&block)
        result, deprecations = collect_deprecations(&block)
        assert deprecations.empty?, "Expected no deprecation warning within the block but received #{deprecations.size}: \n  #{deprecations * "\n  "}"
        result
      end

      def collect_deprecations
        old_behavior = ActiveSupport::Deprecation.behavior
        deprecations = []
        ActiveSupport::Deprecation.behavior = Proc.new do |message, callstack|
          deprecations << message
        end
        result = yield
        [result, deprecations]
      ensure
        ActiveSupport::Deprecation.behavior = old_behavior
      end
    end
  end
end
module ActiveSupport
  module Testing
    # Adds simple access to sample files called file fixtures.
    # File fixtures are normal files stored in
    # <tt>ActiveSupport::TestCase.file_fixture_path</tt>.
    #
    # File fixtures are represented as +Pathname+ objects.
    # This makes it easy to extract specific information:
    #
    #   file_fixture("example.txt").read # get the file's content
    #   file_fixture("example.mp3").size # get the file size
    module FileFixtures
      extend ActiveSupport::Concern

      included do
        class_attribute :file_fixture_path, instance_writer: false
      end

      # Returns a +Pathname+ to the fixture file named +fixture_name+.
      #
      # Raises ArgumentError if +fixture_name+ can't be found.
      def file_fixture(fixture_name)
        path = Pathname.new(File.join(file_fixture_path, fixture_name))

        if path.exist?
          path
        else
          msg = "the directory '%s' does not contain a file named '%s'"
          raise ArgumentError, msg % [file_fixture_path, fixture_name]
        end
      end
    end
  end
end
module ActiveSupport
  module Testing
    module Isolation
      require 'thread'

      def self.included(klass) #:nodoc:
        klass.class_eval do
          parallelize_me!
        end
      end

      def self.forking_env?
        !ENV["NO_FORK"] && Process.respond_to?(:fork)
      end

      @@class_setup_mutex = Mutex.new

      def _run_class_setup      # class setup method should only happen in parent
        @@class_setup_mutex.synchronize do
          unless defined?(@@ran_class_setup) || ENV['ISOLATION_TEST']
            self.class.setup if self.class.respond_to?(:setup)
            @@ran_class_setup = true
          end
        end
      end

      def run
        serialized = run_in_isolation do
          super
        end

        Marshal.load(serialized)
      end

      module Forking
        def run_in_isolation(&blk)
          read, write = IO.pipe
          read.binmode
          write.binmode

          pid = fork do
            read.close
            yield
            write.puts [Marshal.dump(self.dup)].pack("m")
            exit!
          end

          write.close
          result = read.read
          Process.wait2(pid)
          return result.unpack("m")[0]
        end
      end

      module Subprocess
        ORIG_ARGV = ARGV.dup unless defined?(ORIG_ARGV)

        # Crazy H4X to get this working in windows / jruby with
        # no forking.
        def run_in_isolation(&blk)
          require "tempfile"

          if ENV["ISOLATION_TEST"]
            yield
            File.open(ENV["ISOLATION_OUTPUT"], "w") do |file|
              file.puts [Marshal.dump(self.dup)].pack("m")
            end
            exit!
          else
            Tempfile.open("isolation") do |tmpfile|
              env = {
                'ISOLATION_TEST' => self.class.name,
                'ISOLATION_OUTPUT' => tmpfile.path
              }

              load_paths = $-I.map {|p| "-I\"#{File.expand_path(p)}\"" }.join(" ")
              orig_args = ORIG_ARGV.join(" ")
              test_opts = "-n#{self.class.name}##{self.name}"
              command = "#{Gem.ruby} #{load_paths} #{$0} '#{orig_args}' #{test_opts}"

              # IO.popen lets us pass env in a cross-platform way
              child = IO.popen(env, command)

              begin
                Process.wait(child.pid)
              rescue Errno::ECHILD # The child process may exit before we wait
                nil
              end

              return tmpfile.read.unpack("m")[0]
            end
          end
        end
      end

      include forking_env? ? Forking : Subprocess
    end
  end
end
require 'active_support/concern'
require 'active_support/callbacks'

module ActiveSupport
  module Testing
    # Adds support for +setup+ and +teardown+ callbacks.
    # These callbacks serve as a replacement to overwriting the
    # <tt>#setup</tt> and <tt>#teardown</tt> methods of your TestCase.
    #
    #   class ExampleTest < ActiveSupport::TestCase
    #     setup do
    #       # ...
    #     end
    #
    #     teardown do
    #       # ...
    #     end
    #   end
    module SetupAndTeardown
      extend ActiveSupport::Concern

      included do
        include ActiveSupport::Callbacks
        define_callbacks :setup, :teardown
      end

      module ClassMethods
        # Add a callback, which runs before <tt>TestCase#setup</tt>.
        def setup(*args, &block)
          set_callback(:setup, :before, *args, &block)
        end

        # Add a callback, which runs after <tt>TestCase#teardown</tt>.
        def teardown(*args, &block)
          set_callback(:teardown, :after, *args, &block)
        end
      end

      def before_setup # :nodoc:
        super
        run_callbacks :setup
      end

      def after_teardown # :nodoc:
        run_callbacks :teardown
        super
      end
    end
  end
end
module ActiveSupport
  module Testing
    module Stream #:nodoc:
      private

      def silence_stream(stream)
        old_stream = stream.dup
        stream.reopen(IO::NULL)
        stream.sync = true
        yield
      ensure
        stream.reopen(old_stream)
        old_stream.close
      end

      def quietly
        silence_stream(STDOUT) do
          silence_stream(STDERR) do
            yield
          end
        end
      end

      def capture(stream)
        stream = stream.to_s
        captured_stream = Tempfile.new(stream)
        stream_io = eval("$#{stream}")
        origin_stream = stream_io.dup
        stream_io.reopen(captured_stream)

        yield

        stream_io.rewind
        return captured_stream.read
      ensure
        captured_stream.close
        captured_stream.unlink
        stream_io.reopen(origin_stream)
      end
    end
  end
end
module ActiveSupport
  module Testing
    # Logs a "PostsControllerTest: test name" heading before each test to
    # make test.log easier to search and follow along with.
    module TaggedLogging #:nodoc:
      attr_writer :tagged_logger

      def before_setup
        if tagged_logger && tagged_logger.info?
          heading = "#{self.class}: #{name}"
          divider = '-' * heading.size
          tagged_logger.info divider
          tagged_logger.info heading
          tagged_logger.info divider
        end
        super
      end

      private
        def tagged_logger
          @tagged_logger ||= (defined?(Rails.logger) && Rails.logger)
        end
    end
  end
end
module ActiveSupport
  module Testing
    class SimpleStubs # :nodoc:
      Stub = Struct.new(:object, :method_name, :original_method)

      def initialize
        @stubs = {}
      end

      def stub_object(object, method_name, return_value)
        key = [object.object_id, method_name]

        if stub = @stubs[key]
          unstub_object(stub)
        end

        new_name = "__simple_stub__#{method_name}"

        @stubs[key] = Stub.new(object, method_name, new_name)

        object.singleton_class.send :alias_method, new_name, method_name
        object.define_singleton_method(method_name) { return_value }
      end

      def unstub_all!
        @stubs.each_value do |stub|
          unstub_object(stub)
        end
        @stubs = {}
      end

      private

        def unstub_object(stub)
          singleton_class = stub.object.singleton_class
          singleton_class.send :undef_method, stub.method_name
          singleton_class.send :alias_method, stub.method_name, stub.original_method
          singleton_class.send :undef_method, stub.original_method
        end
    end

    # Contain helpers that help you test passage of time.
    module TimeHelpers
      # Changes current time to the time in the future or in the past by a given time difference by
      # stubbing +Time.now+, +Date.today+, and +DateTime.now+.
      #
      #   Time.current     # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      #   travel 1.day
      #   Time.current     # => Sun, 10 Nov 2013 15:34:49 EST -05:00
      #   Date.current     # => Sun, 10 Nov 2013
      #   DateTime.current # => Sun, 10 Nov 2013 15:34:49 -0500
      #
      # This method also accepts a block, which will return the current time back to its original
      # state at the end of the block:
      #
      #   Time.current # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      #   travel 1.day do
      #     User.create.created_at # => Sun, 10 Nov 2013 15:34:49 EST -05:00
      #   end
      #   Time.current # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      def travel(duration, &block)
        travel_to Time.now + duration, &block
      end

      # Changes current time to the given time by stubbing +Time.now+,
      # +Date.today+, and +DateTime.now+ to return the time or date passed into this method.
      #
      #   Time.current     # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      #   travel_to Time.new(2004, 11, 24, 01, 04, 44)
      #   Time.current     # => Wed, 24 Nov 2004 01:04:44 EST -05:00
      #   Date.current     # => Wed, 24 Nov 2004
      #   DateTime.current # => Wed, 24 Nov 2004 01:04:44 -0500
      #
      # Dates are taken as their timestamp at the beginning of the day in the
      # application time zone. <tt>Time.current</tt> returns said timestamp,
      # and <tt>Time.now</tt> its equivalent in the system time zone. Similarly,
      # <tt>Date.current</tt> returns a date equal to the argument, and
      # <tt>Date.today</tt> the date according to <tt>Time.now</tt>, which may
      # be different. (Note that you rarely want to deal with <tt>Time.now</tt>,
      # or <tt>Date.today</tt>, in order to honor the application time zone
      # please always use <tt>Time.current</tt> and <tt>Date.current</tt>.)
      #
      # Note that the usec for the time passed will be set to 0 to prevent rounding
      # errors with external services, like MySQL (which will round instead of floor,
      # leading to off-by-one-second errors).
      #
      # This method also accepts a block, which will return the current time back to its original
      # state at the end of the block:
      #
      #   Time.current # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      #   travel_to Time.new(2004, 11, 24, 01, 04, 44) do
      #     Time.current # => Wed, 24 Nov 2004 01:04:44 EST -05:00
      #   end
      #   Time.current # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      def travel_to(date_or_time)
        if date_or_time.is_a?(Date) && !date_or_time.is_a?(DateTime)
          now = date_or_time.midnight.to_time
        else
          now = date_or_time.to_time.change(usec: 0)
        end

        simple_stubs.stub_object(Time, :now, now)
        simple_stubs.stub_object(Date, :today, now.to_date)
        simple_stubs.stub_object(DateTime, :now, now.to_datetime)

        if block_given?
          begin
            yield
          ensure
            travel_back
          end
        end
      end

      # Returns the current time back to its original state, by removing the stubs added by
      # `travel` and `travel_to`.
      #
      #   Time.current # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      #   travel_to Time.new(2004, 11, 24, 01, 04, 44)
      #   Time.current # => Wed, 24 Nov 2004 01:04:44 EST -05:00
      #   travel_back
      #   Time.current # => Sat, 09 Nov 2013 15:34:49 EST -05:00
      def travel_back
        simple_stubs.unstub_all!
      end

      private

        def simple_stubs
          @simple_stubs ||= SimpleStubs.new
        end
    end
  end
end
module ActiveSupport
  autoload :Duration, 'active_support/duration'
  autoload :TimeWithZone, 'active_support/time_with_zone'
  autoload :TimeZone, 'active_support/values/time_zone'
end

require 'date'
require 'time'

require 'active_support/core_ext/time'
require 'active_support/core_ext/date'
require 'active_support/core_ext/date_time'

require 'active_support/core_ext/integer/time'
require 'active_support/core_ext/numeric/time'

require 'active_support/core_ext/string/conversions'
require 'active_support/core_ext/string/zones'
require 'active_support/values/time_zone'
require 'active_support/core_ext/object/acts_like'

module ActiveSupport
  # A Time-like class that can represent a time in any time zone. Necessary
  # because standard Ruby Time instances are limited to UTC and the
  # system's <tt>ENV['TZ']</tt> zone.
  #
  # You shouldn't ever need to create a TimeWithZone instance directly via +new+.
  # Instead use methods +local+, +parse+, +at+ and +now+ on TimeZone instances,
  # and +in_time_zone+ on Time and DateTime instances.
  #
  #   Time.zone = 'Eastern Time (US & Canada)'        # => 'Eastern Time (US & Canada)'
  #   Time.zone.local(2007, 2, 10, 15, 30, 45)        # => Sat, 10 Feb 2007 15:30:45 EST -05:00
  #   Time.zone.parse('2007-02-10 15:30:45')          # => Sat, 10 Feb 2007 15:30:45 EST -05:00
  #   Time.zone.at(1170361845)                        # => Sat, 10 Feb 2007 15:30:45 EST -05:00
  #   Time.zone.now                                   # => Sun, 18 May 2008 13:07:55 EDT -04:00
  #   Time.utc(2007, 2, 10, 20, 30, 45).in_time_zone  # => Sat, 10 Feb 2007 15:30:45 EST -05:00
  #
  # See Time and TimeZone for further documentation of these methods.
  #
  # TimeWithZone instances implement the same API as Ruby Time instances, so
  # that Time and TimeWithZone instances are interchangeable.
  #
  #   t = Time.zone.now                     # => Sun, 18 May 2008 13:27:25 EDT -04:00
  #   t.hour                                # => 13
  #   t.dst?                                # => true
  #   t.utc_offset                          # => -14400
  #   t.zone                                # => "EDT"
  #   t.to_s(:rfc822)                       # => "Sun, 18 May 2008 13:27:25 -0400"
  #   t + 1.day                             # => Mon, 19 May 2008 13:27:25 EDT -04:00
  #   t.beginning_of_year                   # => Tue, 01 Jan 2008 00:00:00 EST -05:00
  #   t > Time.utc(1999)                    # => true
  #   t.is_a?(Time)                         # => true
  #   t.is_a?(ActiveSupport::TimeWithZone)  # => true
  class TimeWithZone

    # Report class name as 'Time' to thwart type checking.
    def self.name
      'Time'
    end

    include Comparable
    attr_reader :time_zone

    def initialize(utc_time, time_zone, local_time = nil, period = nil)
      @utc, @time_zone, @time = utc_time, time_zone, local_time
      @period = @utc ? period : get_period_and_ensure_valid_local_time(period)
    end

    # Returns a Time or DateTime instance that represents the time in +time_zone+.
    def time
      @time ||= period.to_local(@utc)
    end

    # Returns a Time or DateTime instance that represents the time in UTC.
    def utc
      @utc ||= period.to_utc(@time)
    end
    alias_method :comparable_time, :utc
    alias_method :getgm, :utc
    alias_method :getutc, :utc
    alias_method :gmtime, :utc

    # Returns the underlying TZInfo::TimezonePeriod.
    def period
      @period ||= time_zone.period_for_utc(@utc)
    end

    # Returns the simultaneous time in <tt>Time.zone</tt>, or the specified zone.
    def in_time_zone(new_zone = ::Time.zone)
      return self if time_zone == new_zone
      utc.in_time_zone(new_zone)
    end

    # Returns a <tt>Time.local()</tt> instance of the simultaneous time in your
    # system's <tt>ENV['TZ']</tt> zone.
    def localtime(utc_offset = nil)
      utc.respond_to?(:getlocal) ? utc.getlocal(utc_offset) : utc.to_time.getlocal(utc_offset)
    end
    alias_method :getlocal, :localtime

    # Returns true if the current time is within Daylight Savings Time for the
    # specified time zone.
    #
    #   Time.zone = 'Eastern Time (US & Canada)'    # => 'Eastern Time (US & Canada)'
    #   Time.zone.parse("2012-5-30").dst?           # => true
    #   Time.zone.parse("2012-11-30").dst?          # => false
    def dst?
      period.dst?
    end
    alias_method :isdst, :dst?

    # Returns true if the current time zone is set to UTC.
    #
    #   Time.zone = 'UTC'                           # => 'UTC'
    #   Time.zone.now.utc?                          # => true
    #   Time.zone = 'Eastern Time (US & Canada)'    # => 'Eastern Time (US & Canada)'
    #   Time.zone.now.utc?                          # => false
    def utc?
      time_zone.name == 'UTC'
    end
    alias_method :gmt?, :utc?

    # Returns the offset from current time to UTC time in seconds.
    def utc_offset
      period.utc_total_offset
    end
    alias_method :gmt_offset, :utc_offset
    alias_method :gmtoff, :utc_offset

    # Returns a formatted string of the offset from UTC, or an alternative
    # string if the time zone is already UTC.
    #
    #   Time.zone = 'Eastern Time (US & Canada)'   # => "Eastern Time (US & Canada)"
    #   Time.zone.now.formatted_offset(true)       # => "-05:00"
    #   Time.zone.now.formatted_offset(false)      # => "-0500"
    #   Time.zone = 'UTC'                          # => "UTC"
    #   Time.zone.now.formatted_offset(true, "0")  # => "0"
    def formatted_offset(colon = true, alternate_utc_string = nil)
      utc? && alternate_utc_string || TimeZone.seconds_to_utc_offset(utc_offset, colon)
    end

    # Returns the time zone abbreviation.
    #
    #   Time.zone = 'Eastern Time (US & Canada)'   # => "Eastern Time (US & Canada)"
    #   Time.zone.now.zone # => "EST"
    def zone
      period.zone_identifier.to_s
    end

    # Returns a string of the object's date, time, zone and offset from UTC.
    #
    #   Time.zone.now.httpdate  # => "Thu, 04 Dec 2014 11:00:25 EST -05:00"
    def inspect
      "#{time.strftime('%a, %d %b %Y %H:%M:%S')} #{zone} #{formatted_offset}"
    end

    # Returns a string of the object's date and time in the ISO 8601 standard
    # format.
    #
    #   Time.zone.now.xmlschema  # => "2014-12-04T11:02:37-05:00"
    def xmlschema(fraction_digits = 0)
      fraction = if fraction_digits.to_i > 0
        (".%06i" % time.usec)[0, fraction_digits.to_i + 1]
      end

      "#{time.strftime("%Y-%m-%dT%H:%M:%S")}#{fraction}#{formatted_offset(true, 'Z')}"
    end
    alias_method :iso8601, :xmlschema

    # Coerces time to a string for JSON encoding. The default format is ISO 8601.
    # You can get %Y/%m/%d %H:%M:%S +offset style by setting
    # <tt>ActiveSupport::JSON::Encoding.use_standard_json_time_format</tt>
    # to +false+.
    #
    #   # With ActiveSupport::JSON::Encoding.use_standard_json_time_format = true
    #   Time.utc(2005,2,1,15,15,10).in_time_zone("Hawaii").to_json
    #   # => "2005-02-01T05:15:10.000-10:00"
    #
    #   # With ActiveSupport::JSON::Encoding.use_standard_json_time_format = false
    #   Time.utc(2005,2,1,15,15,10).in_time_zone("Hawaii").to_json
    #   # => "2005/02/01 05:15:10 -1000"
    def as_json(options = nil)
      if ActiveSupport::JSON::Encoding.use_standard_json_time_format
        xmlschema(ActiveSupport::JSON::Encoding.time_precision)
      else
        %(#{time.strftime("%Y/%m/%d %H:%M:%S")} #{formatted_offset(false)})
      end
    end

    def init_with(coder) #:nodoc:
      initialize(coder['utc'], coder['zone'], coder['time'])
    end

    def encode_with(coder) #:nodoc:
      coder.tag = '!ruby/object:ActiveSupport::TimeWithZone'
      coder.map = { 'utc' => utc, 'zone' => time_zone, 'time' => time }
    end

    # Returns a string of the object's date and time in the format used by
    # HTTP requests.
    #
    #   Time.zone.now.httpdate  # => "Tue, 01 Jan 2013 04:39:43 GMT"
    def httpdate
      utc.httpdate
    end

    # Returns a string of the object's date and time in the RFC 2822 standard
    # format.
    #
    #   Time.zone.now.rfc2822  # => "Tue, 01 Jan 2013 04:51:39 +0000"
    def rfc2822
      to_s(:rfc822)
    end
    alias_method :rfc822, :rfc2822

    # Returns a string of the object's date and time.
    # Accepts an optional <tt>format</tt>:
    # * <tt>:default</tt> - default value, mimics Ruby Time#to_s format.
    # * <tt>:db</tt> - format outputs time in UTC :db time. See Time#to_formatted_s(:db).
    # * Any key in <tt>Time::DATE_FORMATS</tt> can be used. See active_support/core_ext/time/conversions.rb.
    def to_s(format = :default)
      if format == :db
        utc.to_s(format)
      elsif formatter = ::Time::DATE_FORMATS[format]
        formatter.respond_to?(:call) ? formatter.call(self).to_s : strftime(formatter)
      else
        "#{time.strftime("%Y-%m-%d %H:%M:%S")} #{formatted_offset(false, 'UTC')}" # mimicking Ruby Time#to_s format
      end
    end
    alias_method :to_formatted_s, :to_s

    # Replaces <tt>%Z</tt> directive with +zone before passing to Time#strftime,
    # so that zone information is correct.
    def strftime(format)
      format = format.gsub(/((?:\A|[^%])(?:%%)*)%Z/, "\\1#{zone}")
      getlocal(utc_offset).strftime(format)
    end

    # Use the time in UTC for comparisons.
    def <=>(other)
      utc <=> other
    end

    # Returns true if the current object's time is within the specified
    # +min+ and +max+ time.
    def between?(min, max)
      utc.between?(min, max)
    end

    # Returns true if the current object's time is in the past.
    def past?
      utc.past?
    end

    # Returns true if the current object's time falls within
    # the current day.
    def today?
      time.today?
    end

    # Returns true if the current object's time is in the future.
    def future?
      utc.future?
    end

    def eql?(other)
      utc.eql?(other)
    end

    def hash
      utc.hash
    end

    # Adds an interval of time to the current object's time and returns that
    # value as a new TimeWithZone object.
    #
    #   Time.zone = 'Eastern Time (US & Canada)' # => 'Eastern Time (US & Canada)'
    #   now = Time.zone.now # => Sun, 02 Nov 2014 01:26:28 EDT -04:00
    #   now + 1000          # => Sun, 02 Nov 2014 01:43:08 EDT -04:00
    #
    # If we're adding a Duration of variable length (i.e., years, months, days),
    # move forward from #time, otherwise move forward from #utc, for accuracy
    # when moving across DST boundaries.
    #
    # For instance, a time + 24.hours will advance exactly 24 hours, while a
    # time + 1.day will advance 23-25 hours, depending on the day.
    #
    #   now + 24.hours      # => Mon, 03 Nov 2014 00:26:28 EST -05:00
    #   now + 1.day         # => Mon, 03 Nov 2014 01:26:28 EST -05:00
    def +(other)
      if duration_of_variable_length?(other)
        method_missing(:+, other)
      else
        result = utc.acts_like?(:date) ? utc.since(other) : utc + other rescue utc.since(other)
        result.in_time_zone(time_zone)
      end
    end
    alias_method :since, :+

    # Returns a new TimeWithZone object that represents the difference between
    # the current object's time and the +other+ time.
    #
    #   Time.zone = 'Eastern Time (US & Canada)' # => 'Eastern Time (US & Canada)'
    #   now = Time.zone.now # => Sun, 02 Nov 2014 01:26:28 EST -05:00
    #   now - 1000          # => Sun, 02 Nov 2014 01:09:48 EST -05:00
    #
    # If subtracting a Duration of variable length (i.e., years, months, days),
    # move backward from #time, otherwise move backward from #utc, for accuracy
    # when moving across DST boundaries.
    #
    # For instance, a time - 24.hours will go subtract exactly 24 hours, while a
    # time - 1.day will subtract 23-25 hours, depending on the day.
    #
    #   now - 24.hours      # => Sat, 01 Nov 2014 02:26:28 EDT -04:00
    #   now - 1.day         # => Sat, 01 Nov 2014 01:26:28 EDT -04:00
    def -(other)
      if other.acts_like?(:time)
        to_time - other.to_time
      elsif duration_of_variable_length?(other)
        method_missing(:-, other)
      else
        result = utc.acts_like?(:date) ? utc.ago(other) : utc - other rescue utc.ago(other)
        result.in_time_zone(time_zone)
      end
    end

    def ago(other)
      since(-other)
    end

    def advance(options)
      # If we're advancing a value of variable length (i.e., years, weeks, months, days), advance from #time,
      # otherwise advance from #utc, for accuracy when moving across DST boundaries
      if options.values_at(:years, :weeks, :months, :days).any?
        method_missing(:advance, options)
      else
        utc.advance(options).in_time_zone(time_zone)
      end
    end

    %w(year mon month day mday wday yday hour min sec usec nsec to_date).each do |method_name|
      class_eval <<-EOV, __FILE__, __LINE__ + 1
        def #{method_name}    # def month
          time.#{method_name} #   time.month
        end                   # end
      EOV
    end

    def to_a
      [time.sec, time.min, time.hour, time.day, time.mon, time.year, time.wday, time.yday, dst?, zone]
    end

    # Returns the object's date and time as a floating point number of seconds
    # since the Epoch (January 1, 1970 00:00 UTC).
    #
    #   Time.zone.now.to_f # => 1417709320.285418
    def to_f
      utc.to_f
    end

    # Returns the object's date and time as an integer number of seconds
    # since the Epoch (January 1, 1970 00:00 UTC).
    #
    #   Time.zone.now.to_i # => 1417709320
    def to_i
      utc.to_i
    end
    alias_method :tv_sec, :to_i

    # Returns the object's date and time as a rational number of seconds
    # since the Epoch (January 1, 1970 00:00 UTC).
    #
    #   Time.zone.now.to_r # => (708854548642709/500000)
    def to_r
      utc.to_r
    end

    # Return an instance of Time in the system timezone.
    def to_time
      utc.to_time
    end

    def to_datetime
      utc.to_datetime.new_offset(Rational(utc_offset, 86_400))
    end

    # So that +self+ <tt>acts_like?(:time)</tt>.
    def acts_like_time?
      true
    end

    # Say we're a Time to thwart type checking.
    def is_a?(klass)
      klass == ::Time || super
    end
    alias_method :kind_of?, :is_a?

    def freeze
      period; utc; time # preload instance variables before freezing
      super
    end

    def marshal_dump
      [utc, time_zone.name, time]
    end

    def marshal_load(variables)
      initialize(variables[0].utc, ::Time.find_zone(variables[1]), variables[2].utc)
    end

    # respond_to_missing? is not called in some cases, such as when type conversion is
    # performed with Kernel#String
    def respond_to?(sym, include_priv = false)
      # ensure that we're not going to throw and rescue from NoMethodError in method_missing which is slow
      return false if sym.to_sym == :to_str
      super
    end

    # Ensure proxy class responds to all methods that underlying time instance
    # responds to.
    def respond_to_missing?(sym, include_priv)
      # consistently respond false to acts_like?(:date), regardless of whether #time is a Time or DateTime
      return false if sym.to_sym == :acts_like_date?
      time.respond_to?(sym, include_priv)
    end

    # Send the missing method to +time+ instance, and wrap result in a new
    # TimeWithZone with the existing +time_zone+.
    def method_missing(sym, *args, &block)
      wrap_with_time_zone time.__send__(sym, *args, &block)
    rescue NoMethodError => e
      raise e, e.message.sub(time.inspect, self.inspect), e.backtrace
    end

    private
      def get_period_and_ensure_valid_local_time(period)
        # we don't want a Time.local instance enforcing its own DST rules as well,
        # so transfer time values to a utc constructor if necessary
        @time = transfer_time_values_to_utc_constructor(@time) unless @time.utc?
        begin
          period || @time_zone.period_for_local(@time)
        rescue ::TZInfo::PeriodNotFound
          # time is in the "spring forward" hour gap, so we're moving the time forward one hour and trying again
          @time += 1.hour
          retry
        end
      end

      def transfer_time_values_to_utc_constructor(time)
        ::Time.utc(time.year, time.month, time.day, time.hour, time.min, time.sec, Rational(time.nsec, 1000))
      end

      def duration_of_variable_length?(obj)
        ActiveSupport::Duration === obj && obj.parts.any? {|p| [:years, :months, :days].include?(p[0]) }
      end

      def wrap_with_time_zone(time)
        if time.acts_like?(:time)
          periods = time_zone.periods_for_local(time)
          self.class.new(nil, time_zone, time, periods.include?(period) ? period : nil)
        elsif time.is_a?(Range)
          wrap_with_time_zone(time.begin)..wrap_with_time_zone(time.end)
        else
          time
        end
      end
  end
end
require 'tzinfo'
require 'thread_safe'
require 'active_support/core_ext/object/blank'
require 'active_support/core_ext/object/try'

module ActiveSupport
  # The TimeZone class serves as a wrapper around TZInfo::Timezone instances.
  # It allows us to do the following:
  #
  # * Limit the set of zones provided by TZInfo to a meaningful subset of 146
  #   zones.
  # * Retrieve and display zones with a friendlier name
  #   (e.g., "Eastern Time (US & Canada)" instead of "America/New_York").
  # * Lazily load TZInfo::Timezone instances only when they're needed.
  # * Create ActiveSupport::TimeWithZone instances via TimeZone's +local+,
  #   +parse+, +at+ and +now+ methods.
  #
  # If you set <tt>config.time_zone</tt> in the Rails Application, you can
  # access this TimeZone object via <tt>Time.zone</tt>:
  #
  #   # application.rb:
  #   class Application < Rails::Application
  #     config.time_zone = 'Eastern Time (US & Canada)'
  #   end
  #
  #   Time.zone      # => #<TimeZone:0x514834...>
  #   Time.zone.name # => "Eastern Time (US & Canada)"
  #   Time.zone.now  # => Sun, 18 May 2008 14:30:44 EDT -04:00
  #
  # The version of TZInfo bundled with Active Support only includes the
  # definitions necessary to support the zones defined by the TimeZone class.
  # If you need to use zones that aren't defined by TimeZone, you'll need to
  # install the TZInfo gem (if a recent version of the gem is installed locally,
  # this will be used instead of the bundled version.)
  class TimeZone
    # Keys are Rails TimeZone names, values are TZInfo identifiers.
    MAPPING = {
      "International Date Line West" => "Pacific/Midway",
      "Midway Island"                => "Pacific/Midway",
      "American Samoa"               => "Pacific/Pago_Pago",
      "Hawaii"                       => "Pacific/Honolulu",
      "Alaska"                       => "America/Juneau",
      "Pacific Time (US & Canada)"   => "America/Los_Angeles",
      "Tijuana"                      => "America/Tijuana",
      "Mountain Time (US & Canada)"  => "America/Denver",
      "Arizona"                      => "America/Phoenix",
      "Chihuahua"                    => "America/Chihuahua",
      "Mazatlan"                     => "America/Mazatlan",
      "Central Time (US & Canada)"   => "America/Chicago",
      "Saskatchewan"                 => "America/Regina",
      "Guadalajara"                  => "America/Mexico_City",
      "Mexico City"                  => "America/Mexico_City",
      "Monterrey"                    => "America/Monterrey",
      "Central America"              => "America/Guatemala",
      "Eastern Time (US & Canada)"   => "America/New_York",
      "Indiana (East)"               => "America/Indiana/Indianapolis",
      "Bogota"                       => "America/Bogota",
      "Lima"                         => "America/Lima",
      "Quito"                        => "America/Lima",
      "Atlantic Time (Canada)"       => "America/Halifax",
      "Caracas"                      => "America/Caracas",
      "La Paz"                       => "America/La_Paz",
      "Santiago"                     => "America/Santiago",
      "Newfoundland"                 => "America/St_Johns",
      "Brasilia"                     => "America/Sao_Paulo",
      "Buenos Aires"                 => "America/Argentina/Buenos_Aires",
      "Montevideo"                   => "America/Montevideo",
      "Georgetown"                   => "America/Guyana",
      "Greenland"                    => "America/Godthab",
      "Mid-Atlantic"                 => "Atlantic/South_Georgia",
      "Azores"                       => "Atlantic/Azores",
      "Cape Verde Is."               => "Atlantic/Cape_Verde",
      "Dublin"                       => "Europe/Dublin",
      "Edinburgh"                    => "Europe/London",
      "Lisbon"                       => "Europe/Lisbon",
      "London"                       => "Europe/London",
      "Casablanca"                   => "Africa/Casablanca",
      "Monrovia"                     => "Africa/Monrovia",
      "UTC"                          => "Etc/UTC",
      "Belgrade"                     => "Europe/Belgrade",
      "Bratislava"                   => "Europe/Bratislava",
      "Budapest"                     => "Europe/Budapest",
      "Ljubljana"                    => "Europe/Ljubljana",
      "Prague"                       => "Europe/Prague",
      "Sarajevo"                     => "Europe/Sarajevo",
      "Skopje"                       => "Europe/Skopje",
      "Warsaw"                       => "Europe/Warsaw",
      "Zagreb"                       => "Europe/Zagreb",
      "Brussels"                     => "Europe/Brussels",
      "Copenhagen"                   => "Europe/Copenhagen",
      "Madrid"                       => "Europe/Madrid",
      "Paris"                        => "Europe/Paris",
      "Amsterdam"                    => "Europe/Amsterdam",
      "Berlin"                       => "Europe/Berlin",
      "Bern"                         => "Europe/Berlin",
      "Rome"                         => "Europe/Rome",
      "Stockholm"                    => "Europe/Stockholm",
      "Vienna"                       => "Europe/Vienna",
      "West Central Africa"          => "Africa/Algiers",
      "Bucharest"                    => "Europe/Bucharest",
      "Cairo"                        => "Africa/Cairo",
      "Helsinki"                     => "Europe/Helsinki",
      "Kyiv"                         => "Europe/Kiev",
      "Riga"                         => "Europe/Riga",
      "Sofia"                        => "Europe/Sofia",
      "Tallinn"                      => "Europe/Tallinn",
      "Vilnius"                      => "Europe/Vilnius",
      "Athens"                       => "Europe/Athens",
      "Istanbul"                     => "Europe/Istanbul",
      "Minsk"                        => "Europe/Minsk",
      "Jerusalem"                    => "Asia/Jerusalem",
      "Harare"                       => "Africa/Harare",
      "Pretoria"                     => "Africa/Johannesburg",
      "Kaliningrad"                  => "Europe/Kaliningrad",
      "Moscow"                       => "Europe/Moscow",
      "St. Petersburg"               => "Europe/Moscow",
      "Volgograd"                    => "Europe/Volgograd",
      "Samara"                       => "Europe/Samara",
      "Kuwait"                       => "Asia/Kuwait",
      "Riyadh"                       => "Asia/Riyadh",
      "Nairobi"                      => "Africa/Nairobi",
      "Baghdad"                      => "Asia/Baghdad",
      "Tehran"                       => "Asia/Tehran",
      "Abu Dhabi"                    => "Asia/Muscat",
      "Muscat"                       => "Asia/Muscat",
      "Baku"                         => "Asia/Baku",
      "Tbilisi"                      => "Asia/Tbilisi",
      "Yerevan"                      => "Asia/Yerevan",
      "Kabul"                        => "Asia/Kabul",
      "Ekaterinburg"                 => "Asia/Yekaterinburg",
      "Islamabad"                    => "Asia/Karachi",
      "Karachi"                      => "Asia/Karachi",
      "Tashkent"                     => "Asia/Tashkent",
      "Chennai"                      => "Asia/Kolkata",
      "Kolkata"                      => "Asia/Kolkata",
      "Mumbai"                       => "Asia/Kolkata",
      "New Delhi"                    => "Asia/Kolkata",
      "Kathmandu"                    => "Asia/Kathmandu",
      "Astana"                       => "Asia/Dhaka",
      "Dhaka"                        => "Asia/Dhaka",
      "Sri Jayawardenepura"          => "Asia/Colombo",
      "Almaty"                       => "Asia/Almaty",
      "Novosibirsk"                  => "Asia/Novosibirsk",
      "Rangoon"                      => "Asia/Rangoon",
      "Bangkok"                      => "Asia/Bangkok",
      "Hanoi"                        => "Asia/Bangkok",
      "Jakarta"                      => "Asia/Jakarta",
      "Krasnoyarsk"                  => "Asia/Krasnoyarsk",
      "Beijing"                      => "Asia/Shanghai",
      "Chongqing"                    => "Asia/Chongqing",
      "Hong Kong"                    => "Asia/Hong_Kong",
      "Urumqi"                       => "Asia/Urumqi",
      "Kuala Lumpur"                 => "Asia/Kuala_Lumpur",
      "Singapore"                    => "Asia/Singapore",
      "Taipei"                       => "Asia/Taipei",
      "Perth"                        => "Australia/Perth",
      "Irkutsk"                      => "Asia/Irkutsk",
      "Ulaanbaatar"                  => "Asia/Ulaanbaatar",
      "Seoul"                        => "Asia/Seoul",
      "Osaka"                        => "Asia/Tokyo",
      "Sapporo"                      => "Asia/Tokyo",
      "Tokyo"                        => "Asia/Tokyo",
      "Yakutsk"                      => "Asia/Yakutsk",
      "Darwin"                       => "Australia/Darwin",
      "Adelaide"                     => "Australia/Adelaide",
      "Canberra"                     => "Australia/Melbourne",
      "Melbourne"                    => "Australia/Melbourne",
      "Sydney"                       => "Australia/Sydney",
      "Brisbane"                     => "Australia/Brisbane",
      "Hobart"                       => "Australia/Hobart",
      "Vladivostok"                  => "Asia/Vladivostok",
      "Guam"                         => "Pacific/Guam",
      "Port Moresby"                 => "Pacific/Port_Moresby",
      "Magadan"                      => "Asia/Magadan",
      "Srednekolymsk"                => "Asia/Srednekolymsk",
      "Solomon Is."                  => "Pacific/Guadalcanal",
      "New Caledonia"                => "Pacific/Noumea",
      "Fiji"                         => "Pacific/Fiji",
      "Kamchatka"                    => "Asia/Kamchatka",
      "Marshall Is."                 => "Pacific/Majuro",
      "Auckland"                     => "Pacific/Auckland",
      "Wellington"                   => "Pacific/Auckland",
      "Nuku'alofa"                   => "Pacific/Tongatapu",
      "Tokelau Is."                  => "Pacific/Fakaofo",
      "Chatham Is."                  => "Pacific/Chatham",
      "Samoa"                        => "Pacific/Apia"
    }

    UTC_OFFSET_WITH_COLON = '%s%02d:%02d'
    UTC_OFFSET_WITHOUT_COLON = UTC_OFFSET_WITH_COLON.tr(':', '')

    @lazy_zones_map = ThreadSafe::Cache.new

    class << self
      # Assumes self represents an offset from UTC in seconds (as returned from
      # Time#utc_offset) and turns this into an +HH:MM formatted string.
      #
      #   TimeZone.seconds_to_utc_offset(-21_600) # => "-06:00"
      def seconds_to_utc_offset(seconds, colon = true)
        format = colon ? UTC_OFFSET_WITH_COLON : UTC_OFFSET_WITHOUT_COLON
        sign = (seconds < 0 ? '-' : '+')
        hours = seconds.abs / 3600
        minutes = (seconds.abs % 3600) / 60
        format % [sign, hours, minutes]
      end

      def find_tzinfo(name)
        TZInfo::Timezone.new(MAPPING[name] || name)
      end

      alias_method :create, :new

      # Returns a TimeZone instance with the given name, or +nil+ if no
      # such TimeZone instance exists. (This exists to support the use of
      # this class with the +composed_of+ macro.)
      def new(name)
        self[name]
      end

      # Returns an array of all TimeZone objects. There are multiple
      # TimeZone objects per time zone, in many cases, to make it easier
      # for users to find their own time zone.
      def all
        @zones ||= zones_map.values.sort
      end

      # Locate a specific time zone object. If the argument is a string, it
      # is interpreted to mean the name of the timezone to locate. If it is a
      # numeric value it is either the hour offset, or the second offset, of the
      # timezone to find. (The first one with that offset will be returned.)
      # Returns +nil+ if no such time zone is known to the system.
      def [](arg)
        case arg
          when String
          begin
            @lazy_zones_map[arg] ||= create(arg)
          rescue TZInfo::InvalidTimezoneIdentifier
            nil
          end
          when Numeric, ActiveSupport::Duration
            arg *= 3600 if arg.abs <= 13
            all.find { |z| z.utc_offset == arg.to_i }
          else
            raise ArgumentError, "invalid argument to TimeZone[]: #{arg.inspect}"
        end
      end

      # A convenience method for returning a collection of TimeZone objects
      # for time zones in the USA.
      def us_zones
        @us_zones ||= all.find_all { |z| z.name =~ /US|Arizona|Indiana|Hawaii|Alaska/ }
      end

      private
        def zones_map
          @zones_map ||= begin
            MAPPING.each_key {|place| self[place]} # load all the zones
            @lazy_zones_map
          end
        end
    end

    include Comparable
    attr_reader :name
    attr_reader :tzinfo

    # Create a new TimeZone object with the given name and offset. The
    # offset is the number of seconds that this time zone is offset from UTC
    # (GMT). Seconds were chosen as the offset unit because that is the unit
    # that Ruby uses to represent time zone offsets (see Time#utc_offset).
    def initialize(name, utc_offset = nil, tzinfo = nil)
      @name = name
      @utc_offset = utc_offset
      @tzinfo = tzinfo || TimeZone.find_tzinfo(name)
      @current_period = nil
    end

    # Returns the offset of this time zone from UTC in seconds.
    def utc_offset
      if @utc_offset
        @utc_offset
      else
        @current_period ||= tzinfo.current_period if tzinfo
        @current_period.utc_offset if @current_period
      end
    end

    # Returns the offset of this time zone as a formatted string, of the
    # format "+HH:MM".
    def formatted_offset(colon=true, alternate_utc_string = nil)
      utc_offset == 0 && alternate_utc_string || self.class.seconds_to_utc_offset(utc_offset, colon)
    end

    # Compare this time zone to the parameter. The two are compared first on
    # their offsets, and then by name.
    def <=>(zone)
      return unless zone.respond_to? :utc_offset
      result = (utc_offset <=> zone.utc_offset)
      result = (name <=> zone.name) if result == 0
      result
    end

    # Compare #name and TZInfo identifier to a supplied regexp, returning +true+
    # if a match is found.
    def =~(re)
      re === name || re === MAPPING[name]
    end

    # Returns a textual representation of this time zone.
    def to_s
      "(GMT#{formatted_offset}) #{name}"
    end

    # Method for creating new ActiveSupport::TimeWithZone instance in time zone
    # of +self+ from given values.
    #
    #   Time.zone = 'Hawaii'                    # => "Hawaii"
    #   Time.zone.local(2007, 2, 1, 15, 30, 45) # => Thu, 01 Feb 2007 15:30:45 HST -10:00
    def local(*args)
      time = Time.utc(*args)
      ActiveSupport::TimeWithZone.new(nil, self, time)
    end

    # Method for creating new ActiveSupport::TimeWithZone instance in time zone
    # of +self+ from number of seconds since the Unix epoch.
    #
    #   Time.zone = 'Hawaii'        # => "Hawaii"
    #   Time.utc(2000).to_f         # => 946684800.0
    #   Time.zone.at(946684800.0)   # => Fri, 31 Dec 1999 14:00:00 HST -10:00
    def at(secs)
      Time.at(secs).utc.in_time_zone(self)
    end

    # Method for creating new ActiveSupport::TimeWithZone instance in time zone
    # of +self+ from parsed string.
    #
    #   Time.zone = 'Hawaii'                   # => "Hawaii"
    #   Time.zone.parse('1999-12-31 14:00:00') # => Fri, 31 Dec 1999 14:00:00 HST -10:00
    #
    # If upper components are missing from the string, they are supplied from
    # TimeZone#now:
    #
    #   Time.zone.now               # => Fri, 31 Dec 1999 14:00:00 HST -10:00
    #   Time.zone.parse('22:30:00') # => Fri, 31 Dec 1999 22:30:00 HST -10:00
    #
    # However, if the date component is not provided, but any other upper
    # components are supplied, then the day of the month defaults to 1:
    #
    #   Time.zone.parse('Mar 2000') # => Wed, 01 Mar 2000 00:00:00 HST -10:00
    def parse(str, now=now())
      parts_to_time(Date._parse(str, false), now)
    end

    # Parses +str+ according to +format+ and returns an ActiveSupport::TimeWithZone.
    #
    # Assumes that +str+ is a time in the time zone +self+,
    # unless +format+ includes an explicit time zone.
    # (This is the same behavior as +parse+.)
    # In either case, the returned TimeWithZone has the timezone of +self+.
    #
    #   Time.zone = 'Hawaii'                   # => "Hawaii"
    #   Time.zone.strptime('1999-12-31 14:00:00', '%Y-%m-%d %H:%M:%S') # => Fri, 31 Dec 1999 14:00:00 HST -10:00
    #
    # If upper components are missing from the string, they are supplied from
    # TimeZone#now:
    #
    #   Time.zone.now                              # => Fri, 31 Dec 1999 14:00:00 HST -10:00
    #   Time.zone.strptime('22:30:00', '%H:%M:%S') # => Fri, 31 Dec 1999 22:30:00 HST -10:00
    #
    # However, if the date component is not provided, but any other upper
    # components are supplied, then the day of the month defaults to 1:
    #
    #   Time.zone.strptime('Mar 2000', '%b %Y') # => Wed, 01 Mar 2000 00:00:00 HST -10:00
    def strptime(str, format, now=now())
      parts_to_time(DateTime._strptime(str, format), now)
    end

    # Returns an ActiveSupport::TimeWithZone instance representing the current
    # time in the time zone represented by +self+.
    #
    #   Time.zone = 'Hawaii'  # => "Hawaii"
    #   Time.zone.now         # => Wed, 23 Jan 2008 20:24:27 HST -10:00
    def now
      time_now.utc.in_time_zone(self)
    end

    # Return the current date in this time zone.
    def today
      tzinfo.now.to_date
    end

    # Returns the next date in this time zone.
    def tomorrow
      today + 1
    end

    # Returns the previous date in this time zone.
    def yesterday
      today - 1
    end

    # Adjust the given time to the simultaneous time in the time zone
    # represented by +self+. Returns a Time.utc() instance -- if you want an
    # ActiveSupport::TimeWithZone instance, use Time#in_time_zone() instead.
    def utc_to_local(time)
      tzinfo.utc_to_local(time)
    end

    # Adjust the given time to the simultaneous time in UTC. Returns a
    # Time.utc() instance.
    def local_to_utc(time, dst=true)
      tzinfo.local_to_utc(time, dst)
    end

    # Available so that TimeZone instances respond like TZInfo::Timezone
    # instances.
    def period_for_utc(time)
      tzinfo.period_for_utc(time)
    end

    # Available so that TimeZone instances respond like TZInfo::Timezone
    # instances.
    def period_for_local(time, dst=true)
      tzinfo.period_for_local(time, dst)
    end

    def periods_for_local(time) #:nodoc:
      tzinfo.periods_for_local(time)
    end

    def init_with(coder) #:nodoc:
      initialize(coder['name'])
    end

    def encode_with(coder) #:nodoc:
      coder.tag ="!ruby/object:#{self.class}"
      coder.map = { 'name' => tzinfo.name }
    end

    private
      def parts_to_time(parts, now)
        return if parts.empty?

        time = Time.new(
          parts.fetch(:year, now.year),
          parts.fetch(:mon, now.month),
          parts.fetch(:mday, parts[:year] || parts[:mon] ? 1 : now.day),
          parts.fetch(:hour, 0),
          parts.fetch(:min, 0),
          parts.fetch(:sec, 0) + parts.fetch(:sec_fraction, 0),
          parts.fetch(:offset, 0)
        )

        if parts[:offset]
          TimeWithZone.new(time.utc, self)
        else
          TimeWithZone.new(nil, self, time)
        end
      end

      def time_now
        Time.now
      end
  end
end
require_relative 'gem_version'

module ActiveSupport
  # Returns the version of the currently loaded ActiveSupport as a <tt>Gem::Version</tt>
  def self.version
    gem_version
  end
end
require 'time'
require 'base64'
require 'bigdecimal'
require 'active_support/core_ext/module/delegation'
require 'active_support/core_ext/string/inflections'
require 'active_support/core_ext/date_time/calculations'

module ActiveSupport
  # = XmlMini
  #
  # To use the much faster libxml parser:
  #   gem 'libxml-ruby', '=0.9.7'
  #   XmlMini.backend = 'LibXML'
  module XmlMini
    extend self

    # This module decorates files deserialized using Hash.from_xml with
    # the <tt>original_filename</tt> and <tt>content_type</tt> methods.
    module FileLike #:nodoc:
      attr_writer :original_filename, :content_type

      def original_filename
        @original_filename || 'untitled'
      end

      def content_type
        @content_type || 'application/octet-stream'
      end
    end

    DEFAULT_ENCODINGS = {
      "binary" => "base64"
    } unless defined?(DEFAULT_ENCODINGS)

    TYPE_NAMES = {
      "Symbol"     => "symbol",
      "Fixnum"     => "integer",
      "Bignum"     => "integer",
      "BigDecimal" => "decimal",
      "Float"      => "float",
      "TrueClass"  => "boolean",
      "FalseClass" => "boolean",
      "Date"       => "date",
      "DateTime"   => "dateTime",
      "Time"       => "dateTime",
      "Array"      => "array",
      "Hash"       => "hash"
    } unless defined?(TYPE_NAMES)

    FORMATTING = {
      "symbol"   => Proc.new { |symbol| symbol.to_s },
      "date"     => Proc.new { |date| date.to_s(:db) },
      "dateTime" => Proc.new { |time| time.xmlschema },
      "binary"   => Proc.new { |binary| ::Base64.encode64(binary) },
      "yaml"     => Proc.new { |yaml| yaml.to_yaml }
    } unless defined?(FORMATTING)

    # TODO use regexp instead of Date.parse
    unless defined?(PARSING)
      PARSING = {
        "symbol"       => Proc.new { |symbol|  symbol.to_s.to_sym },
        "date"         => Proc.new { |date|    ::Date.parse(date) },
        "datetime"     => Proc.new { |time|    Time.xmlschema(time).utc rescue ::DateTime.parse(time).utc },
        "integer"      => Proc.new { |integer| integer.to_i },
        "float"        => Proc.new { |float|   float.to_f },
        "decimal"      => Proc.new { |number|  BigDecimal(number) },
        "boolean"      => Proc.new { |boolean| %w(1 true).include?(boolean.to_s.strip) },
        "string"       => Proc.new { |string|  string.to_s },
        "yaml"         => Proc.new { |yaml|    YAML::load(yaml) rescue yaml },
        "base64Binary" => Proc.new { |bin|     ::Base64.decode64(bin) },
        "binary"       => Proc.new { |bin, entity| _parse_binary(bin, entity) },
        "file"         => Proc.new { |file, entity| _parse_file(file, entity) }
      }

      PARSING.update(
        "double"   => PARSING["float"],
        "dateTime" => PARSING["datetime"]
      )
    end

    delegate :parse, :to => :backend

    def backend
      current_thread_backend || @backend
    end

    def backend=(name)
      backend = name && cast_backend_name_to_module(name)
      self.current_thread_backend = backend if current_thread_backend
      @backend = backend
    end

    def with_backend(name)
      old_backend = current_thread_backend
      self.current_thread_backend = name && cast_backend_name_to_module(name)
      yield
    ensure
      self.current_thread_backend = old_backend
    end

    def to_tag(key, value, options)
      type_name = options.delete(:type)
      merged_options = options.merge(:root => key, :skip_instruct => true)

      if value.is_a?(::Method) || value.is_a?(::Proc)
        if value.arity == 1
          value.call(merged_options)
        else
          value.call(merged_options, key.to_s.singularize)
        end
      elsif value.respond_to?(:to_xml)
        value.to_xml(merged_options)
      else
        type_name ||= TYPE_NAMES[value.class.name]
        type_name ||= value.class.name if value && !value.respond_to?(:to_str)
        type_name   = type_name.to_s   if type_name
        type_name   = "dateTime" if type_name == "datetime"

        key = rename_key(key.to_s, options)

        attributes = options[:skip_types] || type_name.nil? ? { } : { :type => type_name }
        attributes[:nil] = true if value.nil?

        encoding = options[:encoding] || DEFAULT_ENCODINGS[type_name]
        attributes[:encoding] = encoding if encoding

        formatted_value = FORMATTING[type_name] && !value.nil? ?
          FORMATTING[type_name].call(value) : value

        options[:builder].tag!(key, formatted_value, attributes)
      end
    end

    def rename_key(key, options = {})
      camelize  = options[:camelize]
      dasherize = !options.has_key?(:dasherize) || options[:dasherize]
      if camelize
        key = true == camelize ? key.camelize : key.camelize(camelize)
      end
      key = _dasherize(key) if dasherize
      key
    end

    protected

    def _dasherize(key)
      # $2 must be a non-greedy regex for this to work
      left, middle, right = /\A(_*)(.*?)(_*)\Z/.match(key.strip)[1,3]
      "#{left}#{middle.tr('_ ', '--')}#{right}"
    end

    # TODO: Add support for other encodings
    def _parse_binary(bin, entity) #:nodoc:
      case entity['encoding']
      when 'base64'
        ::Base64.decode64(bin)
      else
        bin
      end
    end

    def _parse_file(file, entity)
      f = StringIO.new(::Base64.decode64(file))
      f.extend(FileLike)
      f.original_filename = entity['name']
      f.content_type = entity['content_type']
      f
    end

    private

      def current_thread_backend
        Thread.current[:xml_mini_backend]
      end

      def current_thread_backend=(name)
        Thread.current[:xml_mini_backend] = name && cast_backend_name_to_module(name)
      end

      def cast_backend_name_to_module(name)
        if name.is_a?(Module)
          name
        else
          require "active_support/xml_mini/#{name.downcase}"
          ActiveSupport.const_get("XmlMini_#{name}")
        end
      end
  end

  XmlMini.backend = 'REXML'
end
raise "JRuby is required to use the JDOM backend for XmlMini" unless RUBY_PLATFORM =~ /java/

require 'jruby'
include Java

require 'active_support/core_ext/object/blank'

java_import javax.xml.parsers.DocumentBuilder unless defined? DocumentBuilder
java_import javax.xml.parsers.DocumentBuilderFactory unless defined? DocumentBuilderFactory
java_import java.io.StringReader unless defined? StringReader
java_import org.xml.sax.InputSource unless defined? InputSource
java_import org.xml.sax.Attributes unless defined? Attributes
java_import org.w3c.dom.Node unless defined? Node

module ActiveSupport
  module XmlMini_JDOM #:nodoc:
    extend self

    CONTENT_KEY = '__content__'.freeze

    NODE_TYPE_NAMES = %w{ATTRIBUTE_NODE CDATA_SECTION_NODE COMMENT_NODE DOCUMENT_FRAGMENT_NODE
    DOCUMENT_NODE DOCUMENT_TYPE_NODE ELEMENT_NODE ENTITY_NODE ENTITY_REFERENCE_NODE NOTATION_NODE
    PROCESSING_INSTRUCTION_NODE TEXT_NODE}

    node_type_map = {}
    NODE_TYPE_NAMES.each { |type| node_type_map[Node.send(type)] = type }

    # Parse an XML Document string or IO into a simple hash using Java's jdom.
    # data::
    #   XML Document string or IO to parse
    def parse(data)
      if data.respond_to?(:read)
        data = data.read
      end

      if data.blank?
        {}
      else
        @dbf = DocumentBuilderFactory.new_instance
        # secure processing of java xml
        # http://www.ibm.com/developerworks/xml/library/x-tipcfsx/index.html
        @dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
        @dbf.setFeature("http://xml.org/sax/features/external-general-entities", false)
        @dbf.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
        @dbf.setFeature(javax.xml.XMLConstants::FEATURE_SECURE_PROCESSING, true)
        xml_string_reader = StringReader.new(data)
        xml_input_source = InputSource.new(xml_string_reader)
        doc = @dbf.new_document_builder.parse(xml_input_source)
        merge_element!({CONTENT_KEY => ''}, doc.document_element)
      end
    end

    private

    # Convert an XML element and merge into the hash
    #
    # hash::
    #   Hash to merge the converted element into.
    # element::
    #   XML element to merge into hash
    def merge_element!(hash, element)
      delete_empty(hash)
      merge!(hash, element.tag_name, collapse(element))
    end

    def delete_empty(hash)
      hash.delete(CONTENT_KEY) if hash[CONTENT_KEY] == ''
    end

    # Actually converts an XML document element into a data structure.
    #
    # element::
    #   The document element to be collapsed.
    def collapse(element)
      hash = get_attributes(element)

      child_nodes = element.child_nodes
      if child_nodes.length > 0
        (0...child_nodes.length).each do |i|
          child = child_nodes.item(i)
          merge_element!(hash, child) unless child.node_type == Node.TEXT_NODE
        end
        merge_texts!(hash, element) unless empty_content?(element)
        hash
      else
        merge_texts!(hash, element)
      end
    end

    # Merge all the texts of an element into the hash
    #
    # hash::
    #   Hash to add the converted element to.
    # element::
    #   XML element whose texts are to me merged into the hash
    def merge_texts!(hash, element)
      delete_empty(hash)
      text_children = texts(element)
      if text_children.join.empty?
        hash
      else
        # must use value to prevent double-escaping
        merge!(hash, CONTENT_KEY, text_children.join)
      end
    end

    # Adds a new key/value pair to an existing Hash. If the key to be added
    # already exists and the existing value associated with key is not
    # an Array, it will be wrapped in an Array. Then the new value is
    # appended to that Array.
    #
    # hash::
    #   Hash to add key/value pair to.
    # key::
    #   Key to be added.
    # value::
    #   Value to be associated with key.
    def merge!(hash, key, value)
      if hash.has_key?(key)
        if hash[key].instance_of?(Array)
          hash[key] << value
        else
          hash[key] = [hash[key], value]
        end
      elsif value.instance_of?(Array)
        hash[key] = [value]
      else
        hash[key] = value
      end
      hash
    end

    # Converts the attributes array of an XML element into a hash.
    # Returns an empty Hash if node has no attributes.
    #
    # element::
    #   XML element to extract attributes from.
    def get_attributes(element)
      attribute_hash = {}
      attributes = element.attributes
      (0...attributes.length).each do |i|
         attribute_hash[CONTENT_KEY] ||= ''
         attribute_hash[attributes.item(i).name] =  attributes.item(i).value
      end
      attribute_hash
    end

    # Determines if a document element has text content
    #
    # element::
    #   XML element to be checked.
    def texts(element)
      texts = []
      child_nodes = element.child_nodes
      (0...child_nodes.length).each do |i|
        item = child_nodes.item(i)
        if item.node_type == Node.TEXT_NODE
          texts << item.get_data
        end
      end
      texts
    end

    # Determines if a document element has text content
    #
    # element::
    #   XML element to be checked.
    def empty_content?(element)
      text = ''
      child_nodes = element.child_nodes
      (0...child_nodes.length).each do |i|
        item = child_nodes.item(i)
        if item.node_type == Node.TEXT_NODE
          text << item.get_data.strip
        end
      end
      text.strip.length == 0
    end
  end
end
require 'libxml'
require 'active_support/core_ext/object/blank'
require 'stringio'

module ActiveSupport
  module XmlMini_LibXML #:nodoc:
    extend self

    # Parse an XML Document string or IO into a simple hash using libxml.
    # data::
    #   XML Document string or IO to parse
    def parse(data)
      if !data.respond_to?(:read)
        data = StringIO.new(data || '')
      end

      char = data.getc
      if char.nil?
        {}
      else
        data.ungetc(char)
        LibXML::XML::Parser.io(data).parse.to_hash
      end
    end

  end
end

module LibXML #:nodoc:
  module Conversions #:nodoc:
    module Document #:nodoc:
      def to_hash
        root.to_hash
      end
    end

    module Node #:nodoc:
      CONTENT_ROOT = '__content__'.freeze

      # Convert XML document to hash.
      #
      # hash::
      #   Hash to merge the converted element into.
      def to_hash(hash={})
        node_hash = {}

        # Insert node hash into parent hash correctly.
        case hash[name]
          when Array then hash[name] << node_hash
          when Hash  then hash[name] = [hash[name], node_hash]
          when nil   then hash[name] = node_hash
        end

        # Handle child elements
        each_child do |c|
          if c.element?
            c.to_hash(node_hash)
          elsif c.text? || c.cdata?
            node_hash[CONTENT_ROOT] ||= ''
            node_hash[CONTENT_ROOT] << c.content
          end
        end

        # Remove content node if it is blank
        if node_hash.length > 1 && node_hash[CONTENT_ROOT].blank?
          node_hash.delete(CONTENT_ROOT)
        end

        # Handle attributes
        each_attr { |a| node_hash[a.name] = a.value }

        hash
      end
    end
  end
end

LibXML::XML::Document.include(LibXML::Conversions::Document)
LibXML::XML::Node.include(LibXML::Conversions::Node)
require 'libxml'
require 'active_support/core_ext/object/blank'
require 'stringio'

module ActiveSupport
  module XmlMini_LibXMLSAX #:nodoc:
    extend self

    # Class that will build the hash while the XML document
    # is being parsed using SAX events.
    class HashBuilder

      include LibXML::XML::SaxParser::Callbacks

      CONTENT_KEY   = '__content__'.freeze
      HASH_SIZE_KEY = '__hash_size__'.freeze

      attr_reader :hash

      def current_hash
        @hash_stack.last
      end

      def on_start_document
        @hash = { CONTENT_KEY => '' }
        @hash_stack = [@hash]
      end

      def on_end_document
        @hash = @hash_stack.pop
        @hash.delete(CONTENT_KEY)
      end

      def on_start_element(name, attrs = {})
        new_hash = { CONTENT_KEY => '' }.merge!(attrs)
        new_hash[HASH_SIZE_KEY] = new_hash.size + 1

        case current_hash[name]
          when Array then current_hash[name] << new_hash
          when Hash  then current_hash[name] = [current_hash[name], new_hash]
          when nil   then current_hash[name] = new_hash
        end

        @hash_stack.push(new_hash)
      end

      def on_end_element(name)
        if current_hash.length > current_hash.delete(HASH_SIZE_KEY) && current_hash[CONTENT_KEY].blank? || current_hash[CONTENT_KEY] == ''
          current_hash.delete(CONTENT_KEY)
        end
        @hash_stack.pop
      end

      def on_characters(string)
        current_hash[CONTENT_KEY] << string
      end

      alias_method :on_cdata_block, :on_characters
    end

    attr_accessor :document_class
    self.document_class = HashBuilder

    def parse(data)
      if !data.respond_to?(:read)
        data = StringIO.new(data || '')
      end

      char = data.getc
      if char.nil?
        {}
      else
        data.ungetc(char)

        LibXML::XML::Error.set_handler(&LibXML::XML::Error::QUIET_HANDLER)
        parser = LibXML::XML::SaxParser.io(data)
        document = self.document_class.new

        parser.callbacks = document
        parser.parse
        document.hash
      end
    end
  end
end
begin
  require 'nokogiri'
rescue LoadError => e
  $stderr.puts "You don't have nokogiri installed in your application. Please add it to your Gemfile and run bundle install"
  raise e
end
require 'active_support/core_ext/object/blank'
require 'stringio'

module ActiveSupport
  module XmlMini_Nokogiri #:nodoc:
    extend self

    # Parse an XML Document string or IO into a simple hash using libxml / nokogiri.
    # data::
    #   XML Document string or IO to parse
    def parse(data)
      if !data.respond_to?(:read)
        data = StringIO.new(data || '')
      end

      char = data.getc
      if char.nil?
        {}
      else
        data.ungetc(char)
        doc = Nokogiri::XML(data)
        raise doc.errors.first if doc.errors.length > 0
        doc.to_hash
      end
    end

    module Conversions #:nodoc:
      module Document #:nodoc:
        def to_hash
          root.to_hash
        end
      end

      module Node #:nodoc:
        CONTENT_ROOT = '__content__'.freeze

        # Convert XML document to hash.
        #
        # hash::
        #   Hash to merge the converted element into.
        def to_hash(hash={})
          node_hash = {}

          # Insert node hash into parent hash correctly.
          case hash[name]
            when Array then hash[name] << node_hash
            when Hash  then hash[name] = [hash[name], node_hash]
            when nil   then hash[name] = node_hash
          end

          # Handle child elements
          children.each do |c|
            if c.element?
              c.to_hash(node_hash)
            elsif c.text? || c.cdata?
              node_hash[CONTENT_ROOT] ||= ''
              node_hash[CONTENT_ROOT] << c.content
            end
          end

          # Remove content node if it is blank and there are child tags
          if node_hash.length > 1 && node_hash[CONTENT_ROOT].blank?
            node_hash.delete(CONTENT_ROOT)
          end

          # Handle attributes
          attribute_nodes.each { |a| node_hash[a.node_name] = a.value }

          hash
        end
      end
    end

    Nokogiri::XML::Document.include(Conversions::Document)
    Nokogiri::XML::Node.include(Conversions::Node)
  end
end
begin
  require 'nokogiri'
rescue LoadError => e
  $stderr.puts "You don't have nokogiri installed in your application. Please add it to your Gemfile and run bundle install"
  raise e
end
require 'active_support/core_ext/object/blank'
require 'stringio'

module ActiveSupport
  module XmlMini_NokogiriSAX #:nodoc:
    extend self

    # Class that will build the hash while the XML document
    # is being parsed using SAX events.
    class HashBuilder < Nokogiri::XML::SAX::Document

      CONTENT_KEY   = '__content__'.freeze
      HASH_SIZE_KEY = '__hash_size__'.freeze

      attr_reader :hash

      def current_hash
        @hash_stack.last
      end

      def start_document
        @hash = {}
        @hash_stack = [@hash]
      end

      def end_document
        raise "Parse stack not empty!" if @hash_stack.size > 1
      end

      def error(error_message)
        raise error_message
      end

      def start_element(name, attrs = [])
        new_hash = { CONTENT_KEY => '' }.merge!(Hash[attrs])
        new_hash[HASH_SIZE_KEY] = new_hash.size + 1

        case current_hash[name]
          when Array then current_hash[name] << new_hash
          when Hash  then current_hash[name] = [current_hash[name], new_hash]
          when nil   then current_hash[name] = new_hash
        end

        @hash_stack.push(new_hash)
      end

      def end_element(name)
        if current_hash.length > current_hash.delete(HASH_SIZE_KEY) && current_hash[CONTENT_KEY].blank? || current_hash[CONTENT_KEY] == ''
          current_hash.delete(CONTENT_KEY)
        end
        @hash_stack.pop
      end

      def characters(string)
        current_hash[CONTENT_KEY] << string
      end

      alias_method :cdata_block, :characters
    end

    attr_accessor :document_class
    self.document_class = HashBuilder

    def parse(data)
      if !data.respond_to?(:read)
        data = StringIO.new(data || '')
      end

      char = data.getc
      if char.nil?
        {}
      else
        data.ungetc(char)
        document = self.document_class.new
        parser = Nokogiri::XML::SAX::Parser.new(document)
        parser.parse(data)
        document.hash
      end
    end
  end
end
require 'active_support/core_ext/kernel/reporting'
require 'active_support/core_ext/object/blank'
require 'stringio'

module ActiveSupport
  module XmlMini_REXML #:nodoc:
    extend self

    CONTENT_KEY = '__content__'.freeze

    # Parse an XML Document string or IO into a simple hash.
    #
    # Same as XmlSimple::xml_in but doesn't shoot itself in the foot,
    # and uses the defaults from Active Support.
    #
    # data::
    #   XML Document string or IO to parse
    def parse(data)
      if !data.respond_to?(:read)
        data = StringIO.new(data || '')
      end

      char = data.getc
      if char.nil?
        {}
      else
        data.ungetc(char)
        silence_warnings { require 'rexml/document' } unless defined?(REXML::Document)
        doc = REXML::Document.new(data)

        if doc.root
          merge_element!({}, doc.root)
        else
          raise REXML::ParseException,
            "The document #{doc.to_s.inspect} does not have a valid root"
        end
      end
    end

    private
      # Convert an XML element and merge into the hash
      #
      # hash::
      #   Hash to merge the converted element into.
      # element::
      #   XML element to merge into hash
      def merge_element!(hash, element)
        merge!(hash, element.name, collapse(element))
      end

      # Actually converts an XML document element into a data structure.
      #
      # element::
      #   The document element to be collapsed.
      def collapse(element)
        hash = get_attributes(element)

        if element.has_elements?
          element.each_element {|child| merge_element!(hash, child) }
          merge_texts!(hash, element) unless empty_content?(element)
          hash
        else
          merge_texts!(hash, element)
        end
      end

      # Merge all the texts of an element into the hash
      #
      # hash::
      #   Hash to add the converted element to.
      # element::
      #   XML element whose texts are to me merged into the hash
      def merge_texts!(hash, element)
        unless element.has_text?
          hash
        else
          # must use value to prevent double-escaping
          texts = ''
          element.texts.each { |t| texts << t.value }
          merge!(hash, CONTENT_KEY, texts)
        end
      end

      # Adds a new key/value pair to an existing Hash. If the key to be added
      # already exists and the existing value associated with key is not
      # an Array, it will be wrapped in an Array. Then the new value is
      # appended to that Array.
      #
      # hash::
      #   Hash to add key/value pair to.
      # key::
      #   Key to be added.
      # value::
      #   Value to be associated with key.
      def merge!(hash, key, value)
        if hash.has_key?(key)
          if hash[key].instance_of?(Array)
            hash[key] << value
          else
            hash[key] = [hash[key], value]
          end
        elsif value.instance_of?(Array)
          hash[key] = [value]
        else
          hash[key] = value
        end
        hash
      end

      # Converts the attributes array of an XML element into a hash.
      # Returns an empty Hash if node has no attributes.
      #
      # element::
      #   XML element to extract attributes from.
      def get_attributes(element)
        attributes = {}
        element.attributes.each { |n,v| attributes[n] = v }
        attributes
      end

      # Determines if a document element has text content
      #
      # element::
      #   XML element to be checked.
      def empty_content?(element)
        element.texts.join.blank?
      end
  end
end
