# To spin up local docker postgres instance:

- docker run -d -e POSTGRES_DB=mydb -e POSTGRES_PASSWORD=testpass123 -e POSTGRES_USER=postgres -p "6500:5432" postgres
- URI: postgresql://postgres:testpass123@localhost:6500/mydb

# Do not store cookies domain as "localhost":

Cookies are associated with a specific domain or IP address.

## Gpt explanation using specs 1

When you access your application using localhost or file://, the domain is not explicitly defined,
so the browser treats it as a separate entity from 127.0.0.1. This is why
the cookies are not maintained when you refresh the page using localhost
or file://, but they are maintained when you use 127.0.0.1.

## Gpt explanation using specs (https://curl.se/rfc/cookie_spec.html) 2

The cookie domain "localhost" does not work and causes cookies to be removed on refresh because of the way domain matching works in the cookie specification.

In the cookie specification, the "domain" attribute is used to compare the domain attributes of the cookie with the Internet domain name of the host from which the URL will be fetched. The domain attribute of a cookie is matched against the tail of the fully qualified domain name of the host.

In the case of "localhost", it does not have a fully qualified domain name because it is used to refer to the local machine. Therefore, when a cookie is set with a domain attribute of "localhost", it is not considered a valid domain match when the URL is fetched from the local machine.

As a result, the cookie is not sent in subsequent requests made to the local machine and is effectively removed.

To solve this issue, you can either remove the domain attribute from the cookie or set it to the specific domain or subdomain you are using to access the localhost.

---

127.0.0.1 == localhost
