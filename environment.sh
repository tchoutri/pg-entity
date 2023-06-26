export DB_DATABASE="entity_test"
export DB_HOST="localhost"
export DB_USER="postgres"
export DB_PASSWORD="postgres"
export DB_PORT="5432"

export DB_CONNSTRING="host=${DB_HOST} dbname=${DB_DATABASE} \
  user=${DB_USER} password=${DB_PASSWORD} port=${DB_PORT}"

export PGPASSWORD=${DB_PASSWORD}
