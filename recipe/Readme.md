# Recipe.md
> Explain your codebase while automating it, or simply execute your markdown.

## What is this?
On the surface level, Recipe.md makes your markdown-embedded scripts executable from the command line, 
given they are written in a certain way.

Here is an example:

To `greet` a `person`:

Set `name` to `args[0]`
```sh
echo "Hello, {{ name }}!"
```

This is whats known as a Recipe. 
It starts with the Keyword "To" followed by one or more names in backticks. 
Given that is is loaded, the above recipe can now be executed as `recipe greet person Alice`.

## Roadmap
It is still very early days for this project, 
Eventually, I want this to be a tool that can be used to build powerful custom CLIs for your project, with source code that reads like documentation.
For now, I am still building an MVP. 

## Motivation and Philosophy
Most projects, as they grow and mature, develop their own idiosyncrasies and preferred ways of doing things. 
Often times this ends up becoming tribal knowledge, and as teams evolve is subject to erosion.
A good start is writing down and specifying these, so a newcomer to the project has somewhere to find this information.
But an even better way is encoding your standards more formally. 
People used to argue about code formatting, but nowadays people just run a linter in CI.
There are a lot of steps and boilerplate to remember when creating a new component in Angular, but we can simply run `ng generate component` and have this taken care of in a standardized way.
Recipe aims to be a tool to make it easy for team leaders and seniors to encode standards and correct usage patterns for the things where there isn't a well-maintained third-party framework or linting tool available.
The literate style makes sure that we explain the reasoning behind those standards and patterns, so we don't blindly continue doing things that don't make sense, and it also removes a lot of magic.
The automation makes it easy for newcomers to get off the ground, and provides some motivation to actually write and maintain these documents.

