Overview
--------
ecache is a simple, fast, in-memory hash table for storing arbitrary erlang
terms on a local node. It has a memcached-like interface, complete with expiring
keys.

Installation
------------
    git clone git://github.com/dweldon/ecache.git
    cd ecache && make

Start ecache
------------
    application:start(ecache).

Interface
---------
### Functions
* store(Key, Value)
* store(Key, Value, Timeout)
* load(Key)
* load_all()
* delete(Key)
* flush()
* increment(Key)
* decrement(Key)
* count()

The following examples give an overview of the ecache interface. Please see the
complete documentation by running `make doc`.

### Basic Store and Load
    > ecache:store("car", "honda:2006").
    ok

    > ecache:load("car").
    {ok,"honda:2006"}

### Store with a Timeout
    > ecache:store(<<"fruit">>, ["apples", "oranges"], 10).
    ok

    > ecache:load(<<"fruit">>).
    {ok,["apples","oranges"]}

    %% wait 11 seconds...

    > ecache:load(<<"fruit">>).
    {error,not_found}

### Load All
    > ecache:store(apples, 100).
    ok

    > ecache:store(oranges, 200).
    ok

    > ecache:load_all().
    {ok,[{apples,100},{oranges,200}]}

### Increment and Decrement
    > ecache:store({fruit, <<"apple">>}, 100).
    ok

    > ecache:increment({fruit, <<"apple">>}).
    {ok,101}

    > ecache:decrement({fruit, <<"apple">>}).
    {ok,100}

Notes
-----
### Namespaces
Although namespaces are not explicitly supported by the interface, they can be
achieved by storing a tuple as the key. For example:
`ecache:store({ip, <<”192.168.1.1”>>}, 1).`

### Timeouts
Storing with a timeout of 0 seconds is equivalent to not storing with a timeout. 
Subsequent stores to the same key will always use the most recent timeout value
(if one was given).
