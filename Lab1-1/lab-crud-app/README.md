# Computer Lab Schedule Management System

A Haskell console application for managing computer lab schedules at a university faculty.

## Features

- Manage computer labs (dисплейні класи)
- Manage workstations (робочі місця)
- Manage users (students and instructors)
- Manage scheduled classes
- Track free access usage

## Technical Implementation

This project demonstrates:
- **Type Classes and Instances**: Custom `DatabaseEntity` and `CrudOperations` type classes
- **MySQL Integration**: Using `mysql-simple` library
- **Modular Architecture**: Organized code structure with separate modules
- **Functional Programming**: Pure functional approach with IO monad for side effects

## Project Structure

```
lab-crud-app/
├── stack.yaml              # Stack configuration
├── package.yaml            # Project dependencies
├── app/
│   └── Main.hs            # Main entry point with console menu
└── src/
    ├── Database/
    │   └── Connection.hs  # Database connection utilities
    ├── Models/
    │   ├── Types.hs       # Type class definitions
    │   ├── Lab.hs         # Lab entity
    │   ├── Workstation.hs # Workstation entity
    │   ├── User.hs        # User entity
    │   ├── ScheduledClass.hs # Scheduled class entity
    │   └── FreeAccess.hs  # Free access entity
    └── Operations/
        └── CRUD.hs        # Generic CRUD operations
```

## Prerequisites

- MySQL Server (running on localhost:3306)
- Haskell Stack
- Database `lab_schedule` created with schema from `../schema.sql`

## Quick Start

1. Create the database:
   ```bash
   mysql -u root -p < ../schema.sql
   mysql -u root -p < ../sample_data.sql
   ```

2. Build the project:
   ```bash
   stack build
   ```

3. Run the application:
   ```bash
   stack run
   ```

## Database Configuration

Default configuration (edit `src/Database/Connection.hs` if needed):
- Host: localhost
- Port: 3306
- User: root
- Password: (empty)
- Database: lab_schedule

## Dependencies

- base >= 4.7 && < 5
- mysql-simple >= 0.4.9
- text >= 2.0
- bytestring >= 0.11
- time >= 1.12

## Type Classes

### DatabaseEntity
Basic operations for database entities:
```haskell
class DatabaseEntity a where
    tableName :: a -> String
    selectAll :: Connection -> IO [a]
    selectById :: Connection -> Int -> IO (Maybe a)
```

### CrudOperations
CRUD operations extending DatabaseEntity:
```haskell
class DatabaseEntity a => CrudOperations a where
    insert :: Connection -> a -> IO Int
    update :: Connection -> a -> IO ()
    delete :: Connection -> Int -> IO ()
    display :: a -> IO ()
```

## Usage

The application provides an interactive console menu in Ukrainian with full CRUD operations for all entities.

For detailed usage instructions, see `../INSTRUCTION_UA.md`.

## License

BSD3

## Author

University Lab Work Project
