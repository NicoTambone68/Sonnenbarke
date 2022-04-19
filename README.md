# Sonnenbarke

## _An OTP tuple space management application in Erlang with Ra_

#### Exam project of the course of Distributed Applications and Cloud Computing
#### Masters degree course of Applied Informatics UniUrb 
#### Academic Year 2021/2022

Written by Nicolò Tambone 309828

## Introduction

### Tuple Space

A tuple space is a shared memory space which can be accessed concurrently. This memory space provides a repository for tuples. A set of primitive methods are able to operate on the repository for reading, writing and searching data. A tuple space is a kind of associative memory. An associative memory is accessed by its content and type rather than its address. This kind of memory is suitable to describe parallel algorithms without referring to a specific computer architecture.


### Erlang and OTP

[Erlang](https://www.erlang.org/) is a functional language introduced by Ericsson in the Eighties and designed for reliability and high concurrency. It was originally proprietary software within Ericsson, developed by Joe Armstrong, Robert Virding, and Mike Williams in 1986. It was released as free and open-source software in 1998. Erlang/OTP is supported and maintained by the Open Telecom Platform (OTP) product unit at Ericsson. It runs on a virtual machine called BEAM. Its early application was in telephone switches. Nowadays it's widely used in a range of applications such as web servers and services, instant messaging systems, clients, message brokers and many other where concurrency and reliability are of paramount importance. OTP stands for Open Telecom Platform. It's a wide set of libraries and design principles aimed at building distributed applications that are scalable and fault-tolerant. A detailed overview of OTP is available [here](https://www.erlang.org/doc/design_principles/des_princ.html).

## Project specifications

The TS management module must include the following functions:

### Interface 1

- ```new(name)```
   - Creates a new TS named with name

- ```in(TS, Pattern)```
   - Returns a tuple matching the pattern in the TS and deletes it from the TS
   - Blocks if there is no tuple matching

- ```rd(TS, Pattern)```
   - Returns a tuple matching the pattern in the TS
   - blocks if there is no tuple matching

- ```out(TS, Tuple)```
   - Puts the tuple Tuple in the TS

### Interface 2

- ```in(TS, Pattern, Timeout)```
   - As in(TS, Pattern) but returns after Timeout
   - {ok, Tuple} or {err, timeout}

- ```rd(TS, Pattern, Timeout)```
   - As in rd(TS, Pattern) but returns after Timeout
   - {ok, Tuple} or {err, timeout}


### Interface 3

- ```addNode(TS, Node)```
   - Adds Node to the TS, so Node can access all the tuples of TS

- ```removeNode(TS, Node)```
   - Removes Node from the tuple space TS

- ```nodes(TS)```
   - Shows all the nodes associated with TS

### Pattern handling

A pattern is a tuple with wildcards and values. The wildcard will be represented by the atom *any*

Methods *in* and *rd* must operate on patterns.

## Architectural choices

### Raft algorithm
Raft is a consensus algorithm designed for distributed, fault-tolerant systems. It allows the implementation of cluster systems which are performant and resilient. For a detailed description with an interactive example of the Raft algorithm, please see [here](https://raft.github.io/).

### Ra
Ra is an open-source Erlang implementation of the Raft algorithm. It's one of the most stable and mature projects in this field. Moreover it's well documented and, even if there are not many examples to look at, you can learn it easily with some experimentation. For more details, please refer to the GitHub repository of the [Ra project](https://github.com/rabbitmq/ra).

## Project implementation
The Sonnenbarke application is made of specialized modules. The user interacts with the application through the interface module, called _sbcli_. The main application's module is _sb_ which implements the _ra_machine_ behaviour. It contains all the methods to start and stop the cluster, and to manage and dispatch the messages among the nodes. Another module, called _sbts_ takes care of the tuple space management and contains the implementation of all of the primitives described above. The database operations are performed by the module _sbdbs_. In this release we adopted the _dets_ module to read and write tables datafiles to and from disk. Although very basic and suffering some limitations, such as a maximum manageble table space of 2GB, these dets files are powerful enaugh to satisfy the specifications requirements. Moreover, the application takes care of replicating the data through the nodes, thus adding resilience to the whole system. Actually, a more sophisticated DBMS, such as Mnesia, wuold be doing a better job. This solution will be tested in further developments of this project. The module _sbenv_ reads configurations variables from the file _config/sys-config_. The module _sbcount_ provides unique sequence number to mark every transaction. Last, but not least, the module _sbsystem_ takes care of reading and writing metadata information.

### Notes about Metadata
The Sonnnenbarke application has been designed for robustness. As an architectural choice we have introduced a small metadata table named _sys_meta.dets_ which resides on each node. This table contains the topology of the cluster, plus informations about the stored tuple spaces. 

| Field | Description | Data Type | Remark
| ------ | ------ | ------ | ------ |  
| id       | Identifier | integer() : 1 | Always 1
| name     | Name of the Cluster | string() | Name identifiying the cluster
| status   | Status of the Cluster | atom() : open / closed | Operations allowed only when status =:= open
| last_scn | Last System Change Number | integer() | Unique identifier for transactions
| nodes | Names of the cluster's nodes | [atom()] | List of nodes forming the cluster
| ts | Tuple Spaces | [{string(), [atom()]}  | List of tuples. Every tuple is formed by a tuple space name plus a list of nodes that store the Tuple Space's data

The record describing the above structure is defined by the file _sys_meta.hrl_. Every node stores the very same metadata information in its local _sys_meta.dets_. Whenever a transaction occurs, thus a TS operation which modifies either the structure or the content of data, the change is recorded to the metadata record. If an _in_ or _out_ operation occurs, only the Scn changes in the metadata. If a _new_, _addNode_ or _removeNode_ operation occurs, the subsequent resulting Tuple Space topology will be written to the metadata. Thus the metadata table _sys_meta.dets_ always reflects the current topology of the stored tuple spaces across all the cluster's nodes. Only one node is allowed to write to _sys_meta.dets_, that is the Leader node. (For details about the Leader node, see the Raft algorithm linked in the section above). This is done by means of the Effects callback function of the _ra_machine_ behaviour. The leader node also takes care of updating the metadata informations on the followers nodes. All the follower nodes use _sys_meta.dets_ in a read-only fashion, for internal reference. Every tuple space operation is sent to all nodes, but data are modified only on the nodes that own the tuple space. So, when a node receive a transaction command it first checks _sys_meta.dets_ to see if it owns the tuple space. Otherwise it ignores the command. When a follower is disconnected from the cluster and rejoins after a while, the internal Ra messaging system provides to update it with the missing messages. Thus, also the node's metadata will stay updated.

## Build
#### Requirements: OTP 24
Please note that the current version of Ra requires OTP 24, so the Sonnenbarke application.

#### Building the Sonnenbarke application
We kindly suggest you to use [Rebar3](https://github.com/erlang/rebar3) utility, which is is an Erlang tool aimed to ease the development and release of Erlang libraries and applications and helps to automate repetitive tasks. To compile Sonnenbarke just type the following command:

```
    $ rebar3 do clean, compile
```

# Using Sonnenbarke
To succesfully run a Sonnenbarke Cluster, you need at least three active nodes running, as required by the Raft algorithm by design. The following example is based on the config/sys.config settings of the current repository. Set this file according to your specifications as needed.
```
    $ rebar3 shell --name ra1@<your.own.fully.qualified.domain.name>
```
Open a new shell for each node and start the nodes ra2, ra3 and ra4 with the command above.

## Starting the cluster
Choose one of the open shells and start the cluster with the following command:

```
    (ra4@adcc-lab.example.com)1> sbcli:start().
```

The cluster will perform initialization, showing a series of output messages like the following
```
Sonnenbarke. A tuple space management system with Ra
Nicolò Tambone - UniUrb ADCC
Connecting to node ra1@adcc-lab.example.com: ok
Connecting to node ra2@adcc-lab.example.com: ok
Connecting to node ra3@adcc-lab.example.com: ok
Connecting to node ra4@adcc-lab.example.com: ok
Initializing nodes
Starting Ra
=INFO REPORT==== 19-Apr-2022::15:12:13.183347 ===
    application: ra
    exited: stopped
    type: temporary

Current leader node is 'ra4@adcc-lab.example.com'
All nodes have been started correctly
Cluster started

ok
(ra4@adcc-lab.example.com)2> Cluster state: open
```
## Showing system's metadata
The method _metadata()_ available on the client module _sbcli_ allows to show the current system's metadata. Thus the following command
```
sbcli:metadata().
```
produces an output similar to the following, describing the cluster's metadata according to the structure described above.
```
{ok,{sys_meta,1,my_awesome_cluster,open,2,
              ['ra1@adcc-lab.example.com',
               'ra2@adcc-lab.example.com',
               'ra3@adcc-lab.example.com',
               'ra4@adcc-lab.example.com'],
              [{adcc,['ra4@adcc-lab.example.com']}]}}
```
## Interface 1 commands
### Adding a new Tuple Space
To add a new tuple space named, for example _my_tuple_space_, enter the following command:
```
sbcli:new(my_tuple_space).
```
The system will enter the new tuple space, then will produce an output similar to the following:
```
{ok,{ok,[my_tuple_space,
         'ra4@adcc-lab.example.com']}}
```
Whenever the name of the tuple space was already entered, the cluster will give out the following error message:
```
{err,ts_name_already_exists}
```
### Inserting a tuple in the tuple space
```
sbcli:out(my_tuple_space, {my_tuple, a, b}).
```
Will get the following output:
```
{ok,{ok,{my_tuple,a,b}}}
```

### Reading a tuple 
To read a tuple we are going to use the command rd
```
sbcli:rd(my_tuple_space, {my_tuple, any, any}).
```
If the given tuple exists in the tuple space, we are going to get as an output a list containing all the matching tuples, for example:
```
[{my_tuple,a,b}]
```
Whenever the input tuple doesn't match any of the stored tuples, the interface blocks, as required by the project specifications. As you can see by reading the code, the interruption is implemented by means of the no_reply atom of the gen_server's handle_call function. The process that remains indefinitely waiting is just _sbcli_ on the user's node and the interruption doesn't actually affect at all the cluster, which keeps running. The insertion of a new tuple on the same tuple space from another node, unlocks the client, which will show the following message:
```
Got message new_tuple_in
Waking up after block. Ciao
ok
```
### Reading a tuple and deleting it from the tuple space
The command _in_ works almost identically as rd, with the added collateral of deleting the matching tuple 
```
sbcli:in(my_tuple_space, {my_tuple, any, any}).
```
Will give the following output
```
[{my_tuple,a,b}]
```
Now, performing the same _in_ command as the one above, will cause the client interface to hang, because the original matching pattern has been deleted. As we have seen before, the hanging terminates whenever a new tuple will be inserted in the same tuple space.

## Interface 2 commands
### Reading a tuple with timeout settings
The command rd of the second interface is the same as of that in the above section, with the only difference of an additional timeout parameter. The timeout is expressed in milliseconds. The behaviour of the command is different when it comes across unmatched patterns. In this case, the command will cause the hanging of the client for an amount of time limited by the parameter timeout. For example, the following query, if unmatched, will hang the client for two seconds.
```
sbcli:rd(my_tuple_space, {my_tuple, d, e}, 2000).
```
After two seconds the command will terminate with the following message:
```
{err,timeout}
```
### Reading a tuple with cancellation and timeout settings
The command _in_ works almost exactly as _rd_, with the additional feature of deleting the matching tuple from the tuple space. Whenever the tuple doesn't match, the command will cause the client to hang for an amount of time limited by the parameter time out. For example, the following query, if unmatched, will hang the client for two seconds.
```
sbcli:in(my_tuple_space, {my_tuple, d, e}, 2000).
```
After two seconds the command will terminate with the following message:
```
{err,timeout}
```
## Interface 3 commands
### Adding a node to a tuple space
The following command adds the node _ra3_ to the tuple space _adcc_
```
sbcli:addNode(adcc, 'ra3@example.com').
```
If the command is successful, it will return the following reply
```
{ok,{{addNode,adcc,'ra3@example.com'}, ok}}
```
You can check the new added node using the command metadata:
```
sbcli:metadata().
```
Also you can check the existence of the file _<tuple space name>.dets_ in the _cluster_datafiles_home_dir_ directory for the given node. See _config/sys.config_ for reference.
### Removing a node from a tuple space
The reverse is done by the command removeNode
```
sbcli:removeNode(adcc, 'ra3@example.com').
```
### Listing nodes associated with a tuple space
The command nodes lists all the nodes associated with a given tuple space
```
sbcli:nodes(adcc).
```
The output shows a list of the clusters node on which the given tuple space data are stored.
```
{ok,{ok,['ra3@adcc-lab.example.com',
         'ra4@adcc-lab.example.com']}}
```


# Automated testing
As for the modern best practices of software engineering, automated testing is mandatory. Erlang provides some built-in libraries to ease automated testing. Here we have adopted [Common Test](https://www.erlang.org/doc/apps/common_test/).
The code of the tests is available on the directory test of the repository. To ease the testing procedures we also provide a shell script. Just launch it by moving to the app home dir and typing

```
    $ ./test.sh
```

# Performances
TO DO

# Conclusions
TO DO

# Appendix
## Why is this application named Sonnenbarke?
Well, it is based on Ra which is the name of an open-source implementation of Raft, nevertheless also the name of a divinity of the ancient Egypt: the god Ra who is portrayed half human and half hawk. Ra wanders on two boats, one for the night and one for the day. The boat for the day is the Sun-boat, in german: Sonnenbarke. This is the title of a song of the german band Einstuerzende Neubauten, of whom I'm a massive fan. [Einstuerzende Neubauten, Sonnenbarke (from the album Silence is Sexy, Mute Records, 2000)](https://youtu.be/bcKz1GTZBJg).







