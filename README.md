# Eta Haskell Mock server

This is a [Eta project](https://eta-lang.org/) using Haskell and Gradle, to build a Mock server to be used during the IT phase for Rest connector calls.

This Server is running using the [Eta Gradle plugin](https://github.com/typelead/gradle-eta), and in the lifeCycle is creating and running a Jar over JVM.

### Installation

* Build the project
    ```
    gradle build -PetaSendMetrics=false
    ```
* Run the project
    ```
    gradle run -PetaSendMetrics=false
    ```
**Important** Right now Gradle EtaLang plugin only works with version 4.7/4.8 of Gradle

### Use

The mock server has two endpoints for now, one to set the mock response, and another to mock the response.

#### Set the Mock response

The first uri param is the http response status(404), and the second the delay time(1000)


POST:
```
    http://localhost:3000/setResponse/status/404/delay/1000
```


Body:

```
    {
        "response":"custom_response"
    }
```

#### Consume mock response

The invocation  of this call it will receive the **response body** with a **delay** and **http status code** previously set

GET:
```
    http://localhost:3000/mock/endpoint
```