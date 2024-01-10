-- Script to drop tables and delete schemas for Customer and Customer Items Database

-- Drop tables in CustomerItemsDB schema
DROP TABLE IF EXISTS CustomerItemsDB.ItemImages;
DROP TABLE IF EXISTS CustomerItemsDB.BagItems;
DROP TABLE IF EXISTS CustomerItemsDB.Bags;
DROP TABLE IF EXISTS CustomerItemsDB.BagTypes;

-- Drop tables in CustomerDB schema
DROP TABLE IF EXISTS CustomerDB.Customers;
DROP TABLE IF EXISTS CustomerDB.CustomerPreferences;


-- Drop schemas
DROP SCHEMA IF EXISTS CustomerItemsDB CASCADE;
DROP SCHEMA IF EXISTS CustomerDB CASCADE;