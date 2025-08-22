-- This script prepares the PostgreSQL database and all CRUD functions.
DROP DATABASE IF EXISTS flight_management WITH (FORCE);
DROP USER IF EXISTS flight_user;

CREATE USER flight_user WITH PASSWORD 'flight_password';
CREATE DATABASE flight_management OWNER flight_user;
\c flight_management;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Tables
CREATE TABLE airports ( id UUID PRIMARY KEY DEFAULT uuid_generate_v4(), name VARCHAR(50) NOT NULL UNIQUE, location VARCHAR(100) NOT NULL, max_capacity INTEGER NOT NULL CHECK (max_capacity > 0), created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP );
CREATE TABLE flights ( id UUID PRIMARY KEY DEFAULT uuid_generate_v4(), identifier VARCHAR(10) NOT NULL UNIQUE, origin_airport_id UUID NOT NULL REFERENCES airports(id) ON DELETE RESTRICT, destination_airport_id UUID NOT NULL REFERENCES airports(id) ON DELETE RESTRICT, created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP );
CREATE TABLE controllers ( id UUID PRIMARY KEY DEFAULT uuid_generate_v4(), license_number VARCHAR(20) NOT NULL UNIQUE, name VARCHAR(50) NOT NULL, assigned_flight_id UUID REFERENCES flights(id) ON DELETE SET NULL, experience_years INTEGER NOT NULL DEFAULT 0, created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP, updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP );
ALTER TABLE airports OWNER TO flight_user; ALTER TABLE flights OWNER TO flight_user; ALTER TABLE controllers OWNER TO flight_user;

-- Triggers
CREATE OR REPLACE FUNCTION update_updated_at_column() RETURNS TRIGGER AS $$ BEGIN NEW.updated_at = NOW(); RETURN NEW; END; $$ language 'plpgsql';
CREATE TRIGGER set_airports_updated_at BEFORE UPDATE ON airports FOR EACH ROW EXECUTE PROCEDURE update_updated_at_column();
CREATE TRIGGER set_controllers_updated_at BEFORE UPDATE ON controllers FOR EACH ROW EXECUTE PROCEDURE update_updated_at_column();
CREATE TRIGGER set_flights_updated_at BEFORE UPDATE ON flights FOR EACH ROW EXECUTE PROCEDURE update_updated_at_column();

-- CRUD Functions
CREATE OR REPLACE FUNCTION add_airport(p_name VARCHAR, p_location VARCHAR, p_max_capacity INTEGER) RETURNS BOOLEAN AS $$ INSERT INTO airports (name, location, max_capacity) VALUES (p_name, p_location, p_max_capacity); SELECT true; $$ LANGUAGE SQL;
CREATE OR REPLACE FUNCTION list_airports() RETURNS TABLE(name VARCHAR, location VARCHAR, max_capacity INTEGER) AS $$ SELECT a.name, a.location, a.max_capacity FROM airports a ORDER BY a.name; $$ LANGUAGE SQL;
CREATE OR REPLACE FUNCTION update_airport(p_old_name VARCHAR, p_new_name VARCHAR, p_new_location VARCHAR, p_new_max_capacity INTEGER) RETURNS BOOLEAN AS $$ DECLARE v_rows INTEGER; BEGIN UPDATE airports SET name=p_new_name, location=p_new_location, max_capacity=p_new_max_capacity WHERE name=p_old_name; GET DIAGNOSTICS v_rows=ROW_COUNT; RETURN v_rows>0; END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION delete_airport(p_name VARCHAR) RETURNS BOOLEAN AS $$ DECLARE v_rows INTEGER; BEGIN DELETE FROM airports WHERE name=p_name; GET DIAGNOSTICS v_rows=ROW_COUNT; RETURN v_rows>0; END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION add_controller(p_license VARCHAR, p_name VARCHAR, p_exp INTEGER) RETURNS BOOLEAN AS $$ INSERT INTO controllers (license_number, name, experience_years) VALUES (p_license, p_name, p_exp); SELECT true; $$ LANGUAGE SQL;
CREATE OR REPLACE FUNCTION list_controllers() RETURNS TABLE(license_number VARCHAR, name VARCHAR, experience_years INTEGER) AS $$ SELECT c.license_number, c.name, c.experience_years FROM controllers c ORDER BY c.name; $$ LANGUAGE SQL;
CREATE OR REPLACE FUNCTION update_controller(p_old_license VARCHAR, p_new_name VARCHAR, p_new_exp INTEGER) RETURNS BOOLEAN AS $$ DECLARE v_rows INTEGER; BEGIN UPDATE controllers SET name=p_new_name, experience_years=p_new_exp WHERE license_number=p_old_license; GET DIAGNOSTICS v_rows=ROW_COUNT; RETURN v_rows>0; END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION delete_controller(p_license VARCHAR) RETURNS BOOLEAN AS $$ DECLARE v_rows INTEGER; BEGIN DELETE FROM controllers WHERE license_number=p_license; GET DIAGNOSTICS v_rows=ROW_COUNT; RETURN v_rows>0; END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION add_flight(p_id VARCHAR, p_origin_name VARCHAR, p_dest_name VARCHAR) RETURNS BOOLEAN AS $$ DECLARE v_origin_id UUID; v_dest_id UUID; BEGIN SELECT id INTO v_origin_id FROM airports WHERE name = p_origin_name; IF NOT FOUND THEN RAISE EXCEPTION 'Origin Airport not found: %', p_origin_name; END IF; SELECT id INTO v_dest_id FROM airports WHERE name = p_dest_name; IF NOT FOUND THEN RAISE EXCEPTION 'Destination Airport not found: %', p_dest_name; END IF; IF v_origin_id = v_dest_id THEN RAISE EXCEPTION 'Origin and Destination cannot be the same.'; END IF; INSERT INTO flights (identifier, origin_airport_id, destination_airport_id) VALUES (p_id, v_origin_id, v_dest_id); SELECT true; END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION list_flights() RETURNS TABLE(identifier VARCHAR, origin_name VARCHAR, destination_name VARCHAR) AS $$ SELECT f.identifier, oa.name, da.name FROM flights f JOIN airports oa ON f.origin_airport_id=oa.id JOIN airports da ON f.destination_airport_id=da.id ORDER BY f.identifier; $$ LANGUAGE SQL;
CREATE OR REPLACE FUNCTION update_flight(p_old_id VARCHAR, p_new_origin VARCHAR, p_new_dest VARCHAR) RETURNS BOOLEAN AS $$ DECLARE v_origin_id UUID; v_dest_id UUID; v_rows INTEGER; BEGIN SELECT id INTO v_origin_id FROM airports WHERE name=p_new_origin; IF NOT FOUND THEN RAISE EXCEPTION 'Origin not found: %', p_new_origin; END IF; SELECT id INTO v_dest_id FROM airports WHERE name=p_new_dest; IF NOT FOUND THEN RAISE EXCEPTION 'Destination not found: %', p_new_dest; END IF; UPDATE flights SET origin_airport_id=v_origin_id, destination_airport_id=v_dest_id WHERE identifier=p_old_id; GET DIAGNOSTICS v_rows=ROW_COUNT; RETURN v_rows>0; END; $$ LANGUAGE plpgsql;
CREATE OR REPLACE FUNCTION delete_flight(p_id VARCHAR) RETURNS BOOLEAN AS $$ DECLARE v_rows INTEGER; BEGIN DELETE FROM flights WHERE identifier=p_id; GET DIAGNOSTICS v_rows=ROW_COUNT; RETURN v_rows>0; END; $$ LANGUAGE plpgsql;

-- Permissions
GRANT ALL PRIVILEGES ON TABLE airports, controllers, flights TO flight_user;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO flight_user;
