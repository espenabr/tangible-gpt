# gpt-chat-semiautomation

### Tools for semi-automating interaction with OpenAI's chat completion API

An abstraction layer of building blocks on top of the existing API to simplify automating the conversation.

### Features include
- Prompt GPT for specific interaction modes, e.g. asking you questions or other request for input
- Specification of response format, including prompting and parsing
- Simple command-line integration, but intended as more general-purpose
- Scala client for the standard chat completion HTTP API.

### Purpose
- Save time and effort using GPT in specific ways
- Explore different ways of interacting with GPT

### Design goals
- Ergonomic, composable API
- Decent balance between structured and general-purpose interaction
- Follow the conversation pattern of ChatGPT
- Extensible enough to add new conversation types and response types

### Limitations
- This is just an experiment. I have limited understanding of prompt engineering and prompting the LLM in quite rigid ways might impact the results. With some luck it might be possible to extend this project to mitigate this
- No decent error handling yet
- As you can't always predict the output from LLMs, desired response formats can never be guaranteed

### Getting started

The best way to get s*tarted is looking in the `examples` folder. These demonstrate different use cases and
can hopefully serve as inspiration for what this can be used for.
