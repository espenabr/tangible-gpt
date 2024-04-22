# gpt-chat-semiautomation

### Tools for semi-automating interaction with OpenAI's chat completion API

An abstraction layer of building blocks on top of the existing API to simplify automating the conversation

### Features include

- Prompt GPT for specific interaction modes, e.g. asking you questions or other request for input
- Specification of response format, including prompting and parsing
- Simple command-line integration for playing around, but intended as a library for different applications
- Scala client for the standard chat completion HTTP API
- Simplify GPT4's function calling feature by binding to Scala functions
- Automatically use different strategies to make GPT reason better and improve answers

### Purpose

- Save time and effort using GPT in specific ways
- Explore different ways of interacting with GPT

### Design goals

- Ergonomic, composable API
- Decent balance between structured and general-purpose interaction
- Follow the conversation pattern of ChatGPT
- Extensible enough to add new conversation types and response types

### Limitations

- A bit naive approach to specifying exact response format. Can probably be improved with better prompt engineering
- No decent error handling yet
- As you can't always predict the output from LLMs, desired response formats can never be guaranteed

### Getting started

The best way to get started is looking in the `examples` folder. These demonstrate different use cases and
can hopefully serve as inspiration for what this can be used for.
