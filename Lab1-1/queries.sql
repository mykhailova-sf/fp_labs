-- ============================================================
-- Useful Queries: Lab Utilization and Schedule
-- ============================================================

USE lab_schedule;

-- ============================================================
-- Query 1: View weekly schedule for all labs
-- ============================================================
SELECT
    l.lab_name,
    l.building,
    l.room_number,
    sc.day_of_week,
    sc.start_time,
    sc.end_time,
    sc.course_name,
    u.full_name AS instructor
FROM scheduled_classes sc
JOIN labs l ON sc.lab_id = l.lab_id
JOIN users u ON sc.instructor_id = u.user_id
ORDER BY l.lab_name,
         FIELD(sc.day_of_week, 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'),
         sc.start_time;

-- ============================================================
-- Query 2: Lab utilization - working vs broken workstations
-- ============================================================
SELECT
    l.lab_name,
    l.building,
    l.room_number,
    l.total_workstations,
    COUNT(w.workstation_id) AS total_registered,
    SUM(CASE WHEN w.is_working = TRUE THEN 1 ELSE 0 END) AS working_stations,
    SUM(CASE WHEN w.is_working = FALSE THEN 1 ELSE 0 END) AS broken_stations,
    ROUND(SUM(CASE WHEN w.is_working = TRUE THEN 1 ELSE 0 END) * 100.0 / l.total_workstations, 2) AS availability_percent
FROM labs l
LEFT JOIN workstations w ON l.lab_id = w.lab_id
GROUP BY l.lab_id, l.lab_name, l.building, l.room_number, l.total_workstations
ORDER BY l.lab_name;

-- ============================================================
-- Query 3: Free access usage statistics (last 7 days)
-- ============================================================
SELECT
    l.lab_name,
    u.full_name AS student_name,
    COUNT(fa.access_id) AS visits_count,
    SUM(TIMESTAMPDIFF(MINUTE, fa.start_time, fa.end_time)) AS total_minutes,
    ROUND(AVG(TIMESTAMPDIFF(MINUTE, fa.start_time, fa.end_time)), 2) AS avg_session_minutes
FROM free_access fa
JOIN labs l ON fa.lab_id = l.lab_id
JOIN users u ON fa.user_id = u.user_id
WHERE fa.date >= DATE_SUB(CURDATE(), INTERVAL 7 DAY)
GROUP BY l.lab_id, l.lab_name, u.user_id, u.full_name
ORDER BY total_minutes DESC;

-- ============================================================
-- Query 4: Find available labs on a specific day and time
-- Example: Free labs on Monday at 11:00
-- ============================================================
SELECT
    l.lab_id,
    l.lab_name,
    l.building,
    l.room_number,
    l.total_workstations
FROM labs l
WHERE l.lab_id NOT IN (
    SELECT sc.lab_id
    FROM scheduled_classes sc
    WHERE sc.day_of_week = 'Monday'
      AND '11:00:00' >= sc.start_time
      AND '11:00:00' < sc.end_time
)
ORDER BY l.lab_name;

-- ============================================================
-- Query 5: Instructor schedule
-- ============================================================
SELECT
    u.full_name AS instructor,
    sc.day_of_week,
    sc.start_time,
    sc.end_time,
    sc.course_name,
    l.lab_name,
    l.building,
    l.room_number
FROM users u
JOIN scheduled_classes sc ON u.user_id = sc.instructor_id
JOIN labs l ON sc.lab_id = l.lab_id
WHERE u.user_type = 'instructor'
ORDER BY u.full_name,
         FIELD(sc.day_of_week, 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'),
         sc.start_time;

-- ============================================================
-- Query 6: Most popular labs (by scheduled classes)
-- ============================================================
SELECT
    l.lab_name,
    l.building,
    COUNT(sc.class_id) AS total_scheduled_classes,
    COUNT(DISTINCT sc.instructor_id) AS different_instructors
FROM labs l
LEFT JOIN scheduled_classes sc ON l.lab_id = sc.lab_id
GROUP BY l.lab_id, l.lab_name, l.building
ORDER BY total_scheduled_classes DESC;

-- ============================================================
-- Query 7: Students with most free access time
-- ============================================================
SELECT
    u.full_name AS student_name,
    u.email,
    COUNT(fa.access_id) AS total_visits,
    SUM(TIMESTAMPDIFF(HOUR, fa.start_time, fa.end_time)) AS total_hours
FROM users u
JOIN free_access fa ON u.user_id = fa.user_id
WHERE u.user_type = 'student'
GROUP BY u.user_id, u.full_name, u.email
ORDER BY total_hours DESC;

-- ============================================================
-- Query 8: Daily lab occupancy overview for today
-- ============================================================
SELECT
    l.lab_name,
    sc.start_time,
    sc.end_time,
    sc.course_name,
    u.full_name AS instructor,
    CASE
        WHEN CURTIME() BETWEEN sc.start_time AND sc.end_time THEN 'IN PROGRESS'
        WHEN CURTIME() < sc.start_time THEN 'UPCOMING'
        ELSE 'COMPLETED'
    END AS status
FROM labs l
JOIN scheduled_classes sc ON l.lab_id = sc.lab_id
JOIN users u ON sc.instructor_id = u.user_id
WHERE sc.day_of_week = DAYNAME(CURDATE())
ORDER BY l.lab_name, sc.start_time;
