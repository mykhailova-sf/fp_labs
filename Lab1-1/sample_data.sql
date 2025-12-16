-- ============================================================
-- Sample Data: Schedule of Computer Lab Work at Faculty
-- ============================================================

USE lab_schedule;

-- ============================================================
-- Insert Computer Labs
-- ============================================================
INSERT INTO labs (lab_name, building, room_number, total_workstations) VALUES
('Lab A1', 'Main Building', '101', 20),
('Lab B2', 'Main Building', '205', 15),
('Lab C3', 'Science Building', '301', 25),
('Lab D4', 'Engineering Building', '112', 18);

-- ============================================================
-- Insert Workstations
-- ============================================================
-- Lab A1 workstations (20 computers)
INSERT INTO workstations (lab_id, workstation_number, is_working) VALUES
(1, 1, TRUE), (1, 2, TRUE), (1, 3, TRUE), (1, 4, TRUE), (1, 5, TRUE),
(1, 6, TRUE), (1, 7, TRUE), (1, 8, TRUE), (1, 9, TRUE), (1, 10, TRUE),
(1, 11, TRUE), (1, 12, TRUE), (1, 13, TRUE), (1, 14, TRUE), (1, 15, TRUE),
(1, 16, TRUE), (1, 17, FALSE), (1, 18, TRUE), (1, 19, TRUE), (1, 20, TRUE);

-- Lab B2 workstations (15 computers)
INSERT INTO workstations (lab_id, workstation_number, is_working) VALUES
(2, 1, TRUE), (2, 2, TRUE), (2, 3, TRUE), (2, 4, TRUE), (2, 5, TRUE),
(2, 6, TRUE), (2, 7, TRUE), (2, 8, TRUE), (2, 9, FALSE), (2, 10, TRUE),
(2, 11, TRUE), (2, 12, TRUE), (2, 13, TRUE), (2, 14, TRUE), (2, 15, TRUE);

-- Lab C3 workstations (25 computers)
INSERT INTO workstations (lab_id, workstation_number, is_working) VALUES
(3, 1, TRUE), (3, 2, TRUE), (3, 3, TRUE), (3, 4, TRUE), (3, 5, TRUE),
(3, 6, TRUE), (3, 7, TRUE), (3, 8, TRUE), (3, 9, TRUE), (3, 10, TRUE),
(3, 11, TRUE), (3, 12, TRUE), (3, 13, TRUE), (3, 14, TRUE), (3, 15, TRUE),
(3, 16, TRUE), (3, 17, TRUE), (3, 18, TRUE), (3, 19, TRUE), (3, 20, TRUE),
(3, 21, TRUE), (3, 22, TRUE), (3, 23, TRUE), (3, 24, TRUE), (3, 25, TRUE);

-- Lab D4 workstations (18 computers)
INSERT INTO workstations (lab_id, workstation_number, is_working) VALUES
(4, 1, TRUE), (4, 2, TRUE), (4, 3, TRUE), (4, 4, TRUE), (4, 5, TRUE),
(4, 6, TRUE), (4, 7, TRUE), (4, 8, TRUE), (4, 9, TRUE), (4, 10, TRUE),
(4, 11, TRUE), (4, 12, TRUE), (4, 13, FALSE), (4, 14, TRUE), (4, 15, TRUE),
(4, 16, TRUE), (4, 17, TRUE), (4, 18, TRUE);

-- ============================================================
-- Insert Users (Instructors and Students)
-- ============================================================
-- Instructors
INSERT INTO users (full_name, user_type, email, phone) VALUES
('Petro Ivanov', 'instructor', 'ivanov@university.edu', '+380501234567'),
('Maria Kovalenko', 'instructor', 'kovalenko@university.edu', '+380502345678'),
('Oleh Petrenko', 'instructor', 'petrenko@university.edu', '+380503456789'),
('Natalia Shevchenko', 'instructor', 'shevchenko@university.edu', '+380504567890');

-- Students
INSERT INTO users (full_name, user_type, email, phone) VALUES
('Andrii Sydorenko', 'student', 'sydorenko@student.edu', '+380505678901'),
('Olena Melnyk', 'student', 'melnyk@student.edu', '+380506789012'),
('Dmytro Tkachenko', 'student', 'tkachenko@student.edu', '+380507890123'),
('Anna Bondarenko', 'student', 'bondarenko@student.edu', '+380508901234'),
('Viktor Morozov', 'student', 'morozov@student.edu', '+380509012345'),
('Iryna Pavlenko', 'student', 'pavlenko@student.edu', '+380500123456');

-- ============================================================
-- Insert Scheduled Classes
-- ============================================================
INSERT INTO scheduled_classes (lab_id, instructor_id, course_name, day_of_week, start_time, end_time) VALUES
-- Monday
(1, 1, 'Programming Basics', 'Monday', '08:00:00', '09:30:00'),
(2, 2, 'Database Systems', 'Monday', '10:00:00', '11:30:00'),
(3, 3, 'Web Development', 'Monday', '12:00:00', '13:30:00'),
(1, 4, 'Data Structures', 'Monday', '14:00:00', '15:30:00'),

-- Tuesday
(2, 1, 'Programming Basics', 'Tuesday', '08:00:00', '09:30:00'),
(3, 2, 'Database Systems', 'Tuesday', '10:00:00', '11:30:00'),
(4, 3, 'Operating Systems', 'Tuesday', '12:00:00', '13:30:00'),
(1, 4, 'Algorithms', 'Tuesday', '14:00:00', '15:30:00'),

-- Wednesday
(1, 1, 'Object-Oriented Programming', 'Wednesday', '09:00:00', '10:30:00'),
(2, 2, 'SQL Advanced', 'Wednesday', '11:00:00', '12:30:00'),
(3, 3, 'Web Design', 'Wednesday', '13:00:00', '14:30:00'),
(4, 4, 'Computer Networks', 'Wednesday', '15:00:00', '16:30:00'),

-- Thursday
(3, 1, 'Programming Basics', 'Thursday', '08:00:00', '09:30:00'),
(1, 2, 'Database Design', 'Thursday', '10:00:00', '11:30:00'),
(2, 3, 'Frontend Development', 'Thursday', '12:00:00', '13:30:00'),
(4, 4, 'System Administration', 'Thursday', '14:00:00', '15:30:00'),

-- Friday
(1, 1, 'Final Projects', 'Friday', '09:00:00', '10:30:00'),
(2, 2, 'Database Lab', 'Friday', '11:00:00', '12:30:00'),
(3, 3, 'Web Applications', 'Friday', '13:00:00', '14:30:00'),
(4, 4, 'Network Security', 'Friday', '15:00:00', '16:30:00');

-- ============================================================
-- Insert Free Access Records (Recent activity)
-- ============================================================
INSERT INTO free_access (lab_id, user_id, workstation_id, date, start_time, end_time) VALUES
-- Recent free access sessions
(1, 5, 1, '2025-10-20', '16:00:00', '17:30:00'),
(1, 6, 2, '2025-10-20', '16:00:00', '18:00:00'),
(2, 7, 16, '2025-10-21', '14:00:00', '16:00:00'),
(3, 8, 21, '2025-10-21', '15:00:00', '17:00:00'),
(1, 9, 3, '2025-10-22', '17:00:00', '19:00:00'),
(2, 10, 17, '2025-10-22', '16:30:00', '18:30:00'),
(3, 5, 22, '2025-10-23', '14:00:00', '16:30:00'),
(4, 6, 61, '2025-10-23', '15:00:00', '17:00:00'),
(1, 7, 4, '2025-10-24', '16:00:00', '18:00:00'),
(2, 8, 18, '2025-10-24', '17:00:00', '19:00:00'),
(3, 9, 23, '2025-10-25', '14:30:00', '16:30:00'),
(4, 10, 62, '2025-10-25', '15:30:00', '17:30:00'),
(1, 5, 5, '2025-10-26', '16:00:00', '18:00:00'),
(2, 6, 19, '2025-10-26', '15:00:00', '17:00:00'),
(3, 7, 24, '2025-10-27', '14:00:00', '16:00:00');
