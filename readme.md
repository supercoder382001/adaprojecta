flight_management_hybrid/
|
├── compile.sh                  # Script to build the project
├── run.sh                      # Script to run the compiled application
|
├── flight_management_hybrid.gpr  # The GNAT project file
|
├── sql/                        # Directory for database schema files
│   └── schema.sql
|
├── src/                        # Directory for all Ada source code
│   |
│   ├── config/                 # Configuration files for the application
│   │   ├── database.properties
│   │   └── sync_config.properties
│   |
│   ├── flight_types.ads
│   ├── flight_types.adb
│   |
│   ├── database_operations.ads
│   ├── database_operations.adb
│   |
│   ├── file_operations.ads
│   ├── file_operations.adb
│   |
│   ├── sync_operations.ads
│   ├── sync_operations.adb
│   |
│   └── main.adb                # The main application entry point
|
└── data/                       # Directory for application output (created on first run)
    ├── backups/                # For timestamped backup directories
    └── exports/                # For JSON export files
