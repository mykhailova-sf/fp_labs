-- ============================================================
-- Database Schema: Schedule of Computer Lab Work at Faculty
-- Розклад роботи дисплейних класів факультету
-- ============================================================

DROP DATABASE IF EXISTS lab_schedule;
CREATE DATABASE lab_schedule CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE lab_schedule;

-- ============================================================
-- Table 1: Computer Labs (Дисплейні класи)
-- ============================================================
CREATE TABLE labs (
    lab_id INT PRIMARY KEY AUTO_INCREMENT,
    lab_name VARCHAR(50) NOT NULL,
    building VARCHAR(50) NOT NULL,
    room_number VARCHAR(10) NOT NULL,
    total_workstations INT NOT NULL
) ENGINE=InnoDB;

-- ============================================================
-- Table 2: Workstations (Робочі місця)
-- ============================================================
CREATE TABLE workstations (
    workstation_id INT PRIMARY KEY AUTO_INCREMENT,
    lab_id INT NOT NULL,
    workstation_number INT NOT NULL,
    is_working BOOLEAN DEFAULT TRUE,
    FOREIGN KEY (lab_id) REFERENCES labs(lab_id) ON DELETE CASCADE,
    UNIQUE KEY (lab_id, workstation_number)
) ENGINE=InnoDB;

-- ============================================================
-- Table 3: Users (Користувачі: студенти та викладачі)
-- ============================================================
CREATE TABLE users (
    user_id INT PRIMARY KEY AUTO_INCREMENT,
    full_name VARCHAR(100) NOT NULL,
    user_type ENUM('student', 'instructor') NOT NULL,
    email VARCHAR(100),
    phone VARCHAR(20)
) ENGINE=InnoDB;

-- ============================================================
-- Table 4: Scheduled Classes (Заплановані заняття)
-- ============================================================
CREATE TABLE scheduled_classes (
    class_id INT PRIMARY KEY AUTO_INCREMENT,
    lab_id INT NOT NULL,
    instructor_id INT NOT NULL,
    course_name VARCHAR(100) NOT NULL,
    day_of_week ENUM('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday') NOT NULL,
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    FOREIGN KEY (lab_id) REFERENCES labs(lab_id) ON DELETE CASCADE,
    FOREIGN KEY (instructor_id) REFERENCES users(user_id) ON DELETE CASCADE
) ENGINE=InnoDB;

-- ============================================================
-- Table 5: Free Access Periods (Вільний доступ)
-- ============================================================
CREATE TABLE free_access (
    access_id INT PRIMARY KEY AUTO_INCREMENT,
    lab_id INT NOT NULL,
    user_id INT NOT NULL,
    workstation_id INT NOT NULL,
    date DATE NOT NULL,
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    FOREIGN KEY (lab_id) REFERENCES labs(lab_id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
    FOREIGN KEY (workstation_id) REFERENCES workstations(workstation_id) ON DELETE CASCADE
) ENGINE=InnoDB;

-- ============================================================
-- Indexes for better query performance
-- ============================================================
CREATE INDEX idx_scheduled_day ON scheduled_classes(day_of_week, start_time);
CREATE INDEX idx_free_access_date ON free_access(date, lab_id);
CREATE INDEX idx_users_type ON users(user_type);
