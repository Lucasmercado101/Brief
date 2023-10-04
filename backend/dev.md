# To spin up local docker postgres instance:

- docker run -d -e POSTGRES_DB=mydb -e POSTGRES_PASSWORD=testpass123 -e POSTGRES_USER=postgres -p "6500:5432" postgres
- URI: postgresql://postgres:testpass123@localhost:6500/mydb

# Do not store cookies domain as "localhost":

Cookies are associated with a specific domain or IP address.
When you access your application using localhost or file://, the domain is not explicitly defined,
so the browser treats it as a separate entity from 127.0.0.1. This is why
the cookies are not maintained when you refresh the page using localhost
or file://, but they are maintained when you use 127.0.0.1.

127.0.0.1 == localhost
