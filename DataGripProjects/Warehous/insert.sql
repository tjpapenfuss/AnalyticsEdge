-- Script to create schemas and tables for Customer and Customer Items Database

-- Create schema for Customer Database
CREATE SCHEMA CustomerDB;

-- Create schema for Customer Items Database
CREATE SCHEMA CustomerItemsDB;

-- CustomerDB: Customers Table
CREATE TABLE CustomerDB.Customers (
    CustomerID SERIAL PRIMARY KEY,
    FirstName VARCHAR(255),
    LastName VARCHAR(255),
    Email VARCHAR(255),
    Phone VARCHAR(20),
    Address TEXT,
    AccountCreationDate DATE
);

-- CustomerItemsDB: BagTypes Table
CREATE TABLE CustomerItemsDB.BagTypes (
    BagTypeID SERIAL PRIMARY KEY,
    TypeDescription VARCHAR(255)
);

-- CustomerItemsDB: Bags Table
CREATE TABLE CustomerItemsDB.Bags (
    BagID SERIAL PRIMARY KEY,
    CustomerID INT REFERENCES CustomerDB.Customers(CustomerID),
    BagTypeID INT REFERENCES CustomerItemsDB.BagTypes(BagTypeID),
    RFIDTag VARCHAR(255)
);

-- CustomerItemsDB: BagItems Table
CREATE TABLE CustomerItemsDB.BagItems (
    ItemID SERIAL PRIMARY KEY,
    BagID INT REFERENCES CustomerItemsDB.Bags(BagID),
    ItemDescription TEXT,
    ItemImageURL TEXT
);

-- CustomerItemsDB: ItemImages Table
CREATE TABLE CustomerItemsDB.ItemImages (
    ImageID SERIAL PRIMARY KEY,
    ItemID INT REFERENCES CustomerItemsDB.BagItems(ItemID),
    ImageURL TEXT,
    UploadDate DATE
);

-- Create a new table for Customer Preferences
CREATE TABLE CustomerDB.CustomerPreferences (
    PreferenceID SERIAL PRIMARY KEY,
    Description VARCHAR(255)
);

-- Insert values into CustomerPreferences
INSERT INTO CustomerDB.CustomerPreferences (Description) VALUES
('Reclaiming Living Spaces'),
('Life Transitions'),
('Seasonal Storage'),
('Minimalist Lifestyle'),
('Family Changes'),
('Hobby and Sports Equipment'),
('Collectibles and Keepsakes'),
('Work-Related Storage'),
('Temporary Life Circumstances'),
('Safety and Security');
