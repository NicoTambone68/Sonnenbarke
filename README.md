# Sonnenbarke

## An OTP tuple space management application in Erlang with Ra

#### Exam project of the course of Distributed Applications and Cloud Computing. Academic Year 2021/2022

#### Masters degree course of Applied Informatics UniUrb 

implemented by Nicolò Tambone 309828

## Introduction

### Tuple Space


A tuple space is a shared memory space which can be accessed concurrently. This memory space provides a repository for tuples.
A set of primitive methods are able to operate on the repository for reading, writing and searching data.
A tuple space is a kind of associative memory. An associative memory is accessed by its content and type rather than its address.
This kind of memory is suitable to describe parallel algorithms without referring to a specific computer architecture.


### Erlang and OTP

Erlang is a functional language introduced by Ericsson in the Eighties and designed for reliability and high concurrency.
It runs on a virtual machine called BEAMS. Its early application was in telephone switches. Nowadays it's widely used in a range of applications
such as web servers and services, instant messaging systems, clients, message brokers and many other where concurrency and reliability are of paramount importance.
OTP stands for Open Telecom Platform. It's a wide set of libraries and design principles aimed at building distributed applications that are scalable and fault-tolerant.

A detailed overview of OTP is available [here](https://www.erlang.org/doc/design_principles/des_princ.html).


## Project specifications

The TS management module must include the following functions:

### Interface 1

- new(name) 
   - Creates a new TS named with name

- in(TS, Patttern)
   - Returns a tuple matching the pattern in the TS and deletes it from the TS
   - Blocks if there is no tuple matching

- rd(TS, Pattern)
   - Returns a tuple matching the pattern in the TS
   - blocks if there is no tuple matching

- out(TS, Tuple)
   - Puts the tuple Tuple in the TS

### Interface 2

- in(TS, Pattern, Timeout)
   - As in(TS, Pattern) but returns after Timeout
   - {ok, Tuple} or {err, timeout}

- rd(TS, Pattern, Timeout)
   - As in rd(TS, Pattern) but returns after Timeout
   - {ok, Tuple} or {err, timeout}


### Interface 3

- addNode(TS, Node)
   - Adds Node to the TS, so Node can access all the tuples of TS

- removeNode(TS, Node)
   - Removes Node from the tuple space TS

- nodes(TS)
   - Shows all the nodes associated with TS

### Pattern handling

A pattern is a tuple with wildcards and values. The wildcard will be represented by the atom *any*

Methods *in* and *rd* must operate on patterns.

## Architectural choices

### Raft algorithm

### Ra

Ra is an open source implementation in Erlang of the Raft algorithm. 

## Project implementation

#### Requirements: OTP 24

## Build

    $ rebar3 do clean, compile

# Using Sonnenbarke


# Performances

# Conclusions




