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

- ```in(TS, Patttern)```
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
The Sonnenbarke application is made of specialized modules- The user interacts with the application through the interface module, called _sbcli_. The main application's module is _sb_ which implements the _ra_machine_ behaviour. It contains all the methods to start and stop the cluster, and to manage and dispatch the messages among the nodes. Another module, called _sbts_ takes care of the tuple space management and contains the implementation of all of the primitives described above. The database operations are performed by the module _sbdbs_. In this release we adopted the _dets_ module to read and write tables datafiles to and from disk. Although very basic and suffering some limitations, such as a maximum manageble table space of 2GB, these dets files are powerful enaugh to satisfy the specifications requirements. Moreover, the application takes care of replicating the data through the nodes, thus adding resilience to the whole system. Actually, a more sophisticated DBMS, such as Mnesia, wuold be doing a better job. This solution will be tested in further developments of this project. The module _sbenv_ reads configurations variables from the file _config/sys-config_. The module _sbcount_ provides unique sequence number to mark every transaction. Last, but not least, the module _sbsystem_ takes care of reading and writing metadata information.

### Notes about Metadata
The Sonnnenbarke application has been designed for robustness. As an architectural choice we have introduced a small metadata table named _sys_meta.dets_ which resides on each node. This table contains the topology of the cluster, plus informations about the stored tuple spaces. 

| Field | Description | Data Type
| ------ | ------ | ------ | 
| id       | Identifier | integer() : 1 
| name     | Name of the Cluster | string()
| status   | Status of the Cluster | atom() : open / closed 
| last_scn | Last System Change Number | integer()
| nodes | Names of the cluster's nodes | [atom()]
| ts | Tuple Spaces | [{string(), [atom()]}  


## Build
#### Requirements: OTP 24

    $ rebar3 do clean, compile

# Using Sonnenbarke


# Performances

# Conclusions





