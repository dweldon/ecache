Overview
--------
ecache is a simple, fast, in-memory hash table for storing arbitrary erlang
terms on a local node. It has a memcached-like interface, complete with expiring
keys.

Installation
------------
    git clone git://github.com/dweldon/ecache.git
    cd ecache && make && make test

Start ecache
------------
    application:start(ecache).

Interface
---------
* load(Key)
* store(Key, Value)
* store(Key, Value, Seconds)
* delete(Key)
* increment(Key)
* decrement(Key)
* flush()
* count()

Examples
--------
### basic load and store
    % store information about a car with a simple key
    ecache:store(car, [{make, <<"honda">>}, {year, 2006}]).
    ok
    % load the value out of the cache
    ecache:load(car).
    {ok,[{make,<<"honda">>},{year,2006}]}
    % attempt to load a key that is not in the cache
    ecache:load(honda).
    {error,not_found}

### store with a timeout
    % store with a timeout of 4 seconds from now
    ecache:store(fruit, 100, 4).
    ecache:load(fruit).
    {ok,100}
    % wait 5 seconds...
    ecache:load(fruit).
    {error,not_found}

### increment and decrement
    ecache:store(fruit, 100).
    ecache:increment(fruit).
    {ok,101}
    ecache:increment(fruit).
    {ok,102}
    ecache:decrement(fruit).
    {ok,101}
    ecache:store(fruit, "100").
    % note that increment and decrement only work when the stored value is an integer
    ecache:increment(fruit).   
    {error,not_an_integer}

Notes
-----
### namespaces
Although namespaces are not explicitly supported by the interface, they can be
achieved by storing a tuple as the key. For example:
`ecache:store({ip, <<”192.168.1.1”>>}, 1).`

### timeouts
Storing with a timeout of 0 seconds is equivalent to not storing with a timeout. 
Subsequent stores to the same key will always use the most recent timeout value
(if one was given).

### implementation
ecache stores its data in a private ets table, owned by the ecache server. All
reads and writes are serialized by the server. All data are stored as
`{Key, Value, ExpirationTime}`. During a read, the server will guarantee that if
a key has expired, it will delete the key and return `{error,not_found}`. During
a store, if a timeout is set, an asynchronous message is sent to the purge
server. The purge server keeps track of `{Timeout, Key}` pairs in a priority
queue. Every few seconds, the purge server will look for potentially expired
keys in its queue. (note that a key could have been updated on the ecache server 
after having been placed in the queue - in this case there will be multiple 
entries in the queue for the same key). For each potentially expired key, the 
purge server sends an asynchronous message back to the ecache server to examine 
that key. If the ecache server finds that the key is indeed expired, it will 
delete it from the ets table.
