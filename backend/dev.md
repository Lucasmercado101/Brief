# To spin up local docker postgres instance:

- docker run --name notes-db -d -e POSTGRES_DB=mydb -e POSTGRES_PASSWORD=testpass123 -e POSTGRES_USER=postgres -p "6500:5432" postgres
- URI: postgresql://postgres:testpass123@localhost:6500/mydb

### To access db with psql

- `psql -U postgres -d mydb` in the docker container console
