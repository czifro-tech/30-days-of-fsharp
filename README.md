# 30-days-of-fsharp
30 daily exercises for learning F#

Inspiration
===========

These exercises are ported from [30-day-of-elixir](https://github.com/kblake/30-days-of-elixir). The goal is to simply learn to program in F#. Since elixir and F# have a lot of similarities, it seemed like a good repo to base these exercises off of.

Motivation
==========

I am by no means an expert in F#. I am still learning the intricacies of F#. I will acknowledge that there maybe better implementations of these exercises, but this, for now, is my best attempt. My goal is to get comfortable enough with it for my Master's thesis.

Running
=======

This was created using Xamarin, so it should work just fine in Visual Studio. You can then use VS to run this code. If you use mono, I have provided a make file to build, run, and debug the program.

- `make build-debug` to build in debug mode
- `make debugArgs [option]` to start debugger, then attach to process
- `make runArgs [option]` to start without debugger and pass args to program. I know this is not good practice
