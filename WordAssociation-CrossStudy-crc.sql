-- Uncomment the following line is building a SQLite database
-- PRAGMA foreign_keys = ON;

USE `associations-db`;

CREATE TABLE researchers (
  researcher_id INTEGER PRIMARY KEY,
  researcher_first_name TEXT NOT NULL,
  researcher_last_name TEXT NOT NULL,
  researcher_email TEXT NOT NULL UNIQUE
);

INSERT INTO researchers (researcher_id, researcher_first_name, researcher_last_name, researcher_email)
VALUES
  (1, 'Stan',     'West',    'swest19@lsu.edu'),
  (2, 'Chris',    'Cox',     'chriscox@lsu.edu'),
  (3, 'Meghan',  'Garcelon', 'mgarce3@lsu.edu'),
  (4, 'Angelina', 'Chauvin',  'achau22@lsu.edu'),
  (5, 'Hannah', 'Pedigo', 'hpedig2@lsu.edu'),
  (6, 'Sophie', 'Vidrine', 'svidri8@lsu.edu'),
  (7, 'Francesca', 'Thomassee', 'fthom22@lsu.edu'),
  (8, 'Erin',     'Jines',    'ejines1@lsu.edu'),
  (9, 'Trinity',    'Phipps',     'tphipp4@lsu.edu'),
  (10, 'Daniela',  'Ucles', 'ducles1@lsu.edu'),
  (11, 'Marissa', 'Goldthorp',  'mgoldt1@lsu.edu'),
  (12, 'Camila', 'Astete', 'castet2@lsu.edu'),
  (13, 'Layla', 'Canaday', 'lcanad2@lsu.edu')
;

CREATE TABLE education_levels (
  education_level_id INTEGER PRIMARY KEY NOT NULL,
  education_level_code TEXT,
  education_level_text TEXT
);

CREATE TABLE gender_identities (
  gender_identity_id INTEGER PRIMARY KEY NOT NULL,
  gender_identity_code TEXT,
  gender_identity_text TEXT
);

CREATE TABLE income_levels (
  income_level_id INTEGER PRIMARY KEY NOT NULL,
  income_level_code TEXT,
  income_level_text TEXT
);

CREATE TABLE racial_identities (
  racial_identity_id INTEGER PRIMARY KEY NOT NULL,
  racial_identity_code TEXT,
  racial_identity_text TEXT
);

CREATE TABLE ethnic_identities (
  ethnic_identity_id INTEGER PRIMARY KEY NOT NULL,
  ethnic_identity_code TEXT,
  ethnic_identity_text TEXT
);

CREATE TABLE toddler_interaction_questions (
  toddler_interaction_question_id INTEGER PRIMARY KEY NOT NULL,
  toddler_interaction_question_code TEXT NOT NULL,
  toddler_interaction_question_text TEXT NOT NULL
);

INSERT INTO toddler_interaction_questions (
  toddler_interaction_question_id,
  toddler_interaction_question_code,
  toddler_interaction_question_text
)
VALUES
  (1, 'is_parent_of_toddler', 'Are you currently the parent or guardian of a toddler?'),
  (2, 'was_parent_of_toddler', 'Are you a parent or guardian who raised a toddler?'),
  (3, 'not_parent_interaction', 'Do you ever interact with toddlers? Perhaps as an aunt, uncle, grandparent, teacher, therapist, coach, or as a friend of parents with a toddler.'),
  (4, 'not_parent_interaction_frequency', 'How often do you interact with toddlers?')
;

CREATE TABLE toddler_interaction_valid_responses (
  toddler_interaction_valid_response_id INTEGER PRIMARY KEY NOT NULL,
  toddler_interaction_valid_response_text TEXT NOT NULL,
  toddler_interaction_valid_response_score INTEGER,
  toddler_interaction_question_id INTEGER,
  FOREIGN KEY (toddler_interaction_question_id) REFERENCES toddler_interaction_questions(toddler_interaction_question_id)
);

INSERT INTO toddler_interaction_valid_responses (
  toddler_interaction_valid_response_id,
  toddler_interaction_valid_response_text,
  toddler_interaction_valid_response_score,
  toddler_interaction_question_id
)
VALUES
  ( 1, 'no',      0, 1),
  ( 2, 'yes',     1, 1),
  ( 3, 'no',      0, 2),
  ( 4, 'yes',     1, 2),
  ( 5, 'no',      0, 3),
  ( 6, 'yes',     1, 3),
  ( 7, 'rarely',  0, 4),
  ( 8, 'monthly', 1, 4),
  ( 9, 'weekly',  2, 4),
  (10, 'daily',   3, 4)
;

CREATE TABLE toddler_interactions (
  toddler_interaction_id INTEGER PRIMARY KEY NOT NULL,
  toddler_interaction_question_id INTEGER NOT NULL,
  toddler_interaction_valid_response_id INTEGER NOT NULL,
  FOREIGN KEY (toddler_interaction_question_id) REFERENCES toddler_interaction_questions(toddler_interaction_question_id),
  FOREIGN KEY (toddler_interaction_valid_response_id) REFERENCES toddler_interaction_valid_responses(toddler_interaction_valid_response_id)
);

CREATE TABLE assert_questions (
  assert_question_id INTEGER PRIMARY KEY NOT NULL,
  assert_question_code TEXT,
  assert_question_text TEXT
);

INSERT INTO assert_questions (assert_question_id, assert_question_code, assert_question_text)
VALUES
  (1, 'S1', 'Do you find it difficult to socialize with, or to get in touch with people, especially people your own age?'),
  (2, 'S2', 'Do you prefer to be alone rather than being together with other people?'),
  (3, 'S3', 'Do you have difficulties perceiving social cues?'),
  (4, 'S4', 'Do other people tell you that your behavior or your emotional responses are inappropriate or hurtful?'),
  (5, 'R1', 'Do you have a strong interest or hobby that absorbs so much of your time that it hampers other activities?'),
  (6, 'R2', 'Do you or do other people feel that you have very set routines or that you are very immersed in your own interests?'),
  (7, 'R3', 'Do you or do other people feel that you impose your routines or interests on others?')
;

CREATE TABLE assert_valid_responses (
  assert_valid_response_id INTEGER PRIMARY KEY NOT NULL,
  assert_valid_response_text TEXT NOT NULL,
  assert_valid_response_score INTEGER NOT NULL,
  assert_question_id INTEGER,
  FOREIGN KEY (assert_question_id) REFERENCES assert_questions(assert_question_id)
);

INSERT INTO assert_valid_responses (
  assert_valid_response_id,
  assert_valid_response_text,
  assert_valid_response_score,
  assert_question_id
)
VALUES
  ( 1, 'not true',       0, 1),
  ( 2, 'somewhat true',  1, 1),
  ( 3, 'certainly true', 2, 1),
  ( 4, 'not true',       0, 2),
  ( 5, 'somewhat true',  1, 2),
  ( 6, 'certainly true', 2, 2),
  ( 7, 'not true',       0, 3),
  ( 8, 'somewhat true',  1, 3),
  ( 9, 'certainly true', 2, 3),
  (10, 'not true',       0, 4),
  (11, 'somewhat true',  1, 4),
  (12, 'certainly true', 2, 4),
  (13, 'not true',       0, 5),
  (14, 'somewhat true',  1, 5),
  (15, 'certainly true', 2, 5),
  (16, 'not true',       0, 6),
  (17, 'somewhat true',  1, 6),
  (18, 'certainly true', 2, 6),
  (19, 'not true',       0, 7),
  (20, 'somewhat true',  1, 7),
  (21, 'certainly true', 2, 7)
;

CREATE TABLE assert (
  assert_id INTEGER PRIMARY KEY NOT NULL,
  assert_question_id INTEGER NOT NULL,
  assert_valid_response_id INTEGER NOT NULL,
  FOREIGN KEY (assert_question_id) REFERENCES assert_questions(assert_question_id),
  FOREIGN KEY (assert_valid_response_id) REFERENCES assert_valid_responses(assert_valid_response_id)
);

CREATE TABLE autism_identities (
  autism_identity_id INTEGER PRIMARY KEY NOT NULL,
  autism_identity_text TEXT NOT NULL UNIQUE,
  identify_autistic TEXT NOT NULL,
  diagnosed_autistic TEXT NOT NULL
);


INSERT INTO autism_identities (
  autism_identity_id,
  identify_autistic,
  diagnosed_autistic,
  autism_identity_text
)
VALUES
  (1, 'yes',   'yes', 'Yes. I have received a diagnosis of Autism Spectrum Disorder.'),
  (2, 'yes',   'no',  'Yes. I have not been formally diagnosed with an Autism Spectrum Disorder, but I identify as Autistic.'),
  (3, 'no',    'yes', 'I have been diagnosed with an Autism Spectrum Disorder, but I do not identify as Autistic.'),
  (4, 'maybe', 'yes', 'It''s complicated. I have been diagnosed with an Autism Spectrum Disorder, but I am not sure if I agree.'),
  (5, 'no',    'no',  'No. I have not been diagnosed with an Autism Spectrum Disorder and I do not identify as Autistic.')
;

CREATE TABLE kuperman (
  kuperman_id INTEGER PRIMARY KEY NOT NULL,
  word TEXT NOT NULL,
  word_alt_spelling TEXT NOT NULL,
  lemma TEXT NOT NULL,
  part_of_speech TEXT NOT NULL,
  word_aoa DOUBLE,
  lemma_aoa DOUBLE
);


CREATE TABLE subtlex (
  subtlex_id INTEGER PRIMARY KEY NOT NULL,
  word TEXT NOT NULL,
  Lg10WF DOUBLE NOT NULL,
  Lg10CD DOUBLE NOT NULL
);


CREATE TABLE conditions (
  condition_id INTEGER PRIMARY KEY NOT NULL,
  condition_code TEXT NOT NULL,
  condition_desc TEXT NOT NULL
);

CREATE TABLE studies (
  study_id INTEGER PRIMARY KEY NOT NULL,
  study_code TEXT NOT NULL UNIQUE,
  study_name TEXT NOT NULL UNIQUE,
  study_desc TEXT
);

CREATE TABLE responses (
  response_id INTEGER PRIMARY KEY NOT NULL,
  response_text TEXT NOT NULL UNIQUE
);

-- TODO: map cues to information in other databases
CREATE TABLE cues (
  cue_id INTEGER PRIMARY KEY NOT NULL,
  cue_text TEXT NOT NULL UNIQUE
);


CREATE TABLE subject_quality (
  subject_quality_id INTEGER PRIMARY KEY NOT NULL,
  subject_quality_code TEXT,
  subject_quality_desc TEXT
);

INSERT INTO subject_quality (subject_quality_id, subject_quality_code, subject_quality_desc)
VALUES
  (1, 'ideal', 'Completed the task conscientiously'),
  (2, 'fine',  'Completed the task mostly well, but with some careless or low effort behavior'),
  (3, 'careless',  'Completed the task, but with evident lack of care throughout'),
  (4, 'bad', 'Completed the task, but responses are garbage'),
  (5, 'incomplete', 'The task is incomplete')
;


CREATE TABLE subjects (
  subject_id INTEGER PRIMARY KEY NOT NULL,
  subject_code TEXT NOT NULL,
  age_years INTEGER,
  study_id INTEGER NOT NULL,
  condition_id INTEGER NOT NULL,
  subject_quality_id INTEGER,
  education_level_id INTEGER,
  income_level_id INTEGER,
  gender_identity_id INTEGER,
  racial_identity_id INTEGER,
  ethnic_identity_id INTEGER,
  autism_identity_id INTEGER,
  assert_id INTEGER,
  toddler_interaction_id INTEGER,
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (condition_id) REFERENCES conditions(condition_id),
  FOREIGN KEY (subject_quality_id) REFERENCES subject_quality(subject_quality_id),
  FOREIGN KEY (education_level_id) REFERENCES education_levels(education_level_id),
  FOREIGN KEY (income_level_id) REFERENCES income_levels(income_level_id),
  FOREIGN KEY (gender_identity_id) REFERENCES gender_identities(gender_identity_id),
  FOREIGN KEY (racial_identity_id) REFERENCES racial_identities(racial_identity_id),
  FOREIGN KEY (ethnic_identity_id) REFERENCES ethnic_identities(ethnic_identity_id),
  FOREIGN KEY (autism_identity_id) REFERENCES autism_identities(autism_identity_id),
  FOREIGN KEY (assert_id) REFERENCES assert(assert_id),
  FOREIGN KEY (toddler_interaction_id) REFERENCES toddler_interactions(toddler_interaction_id)
);

CREATE TABLE associations (
  association_id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  subject_id INTEGER NOT NULL,
  condition_id INTEGER,
  cue_id INTEGER NOT NULL,
  cue_order INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  response_order INTEGER NOT NULL,
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (subject_id) REFERENCES subjects(subject_id),
  FOREIGN KEY (cue_id) REFERENCES cues(cue_id),
  FOREIGN KEY (response_id) REFERENCES responses(response_id)
);


CREATE TABLE study_cue_responses (
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  PRIMARY KEY (study_id, cue_id, response_id),
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (cue_id) REFERENCES cues(cue_id),
  FOREIGN KEY (response_id) REFERENCES responses(response_id)
);

CREATE TABLE subject_locks (
  subject_lock_id INTEGER PRIMARY KEY,
  subject_lock_hash TEXT NOT NULL UNIQUE,
  subject_lock_owner_researcher_id INTEGER NOT NULL,
  subject_lock_time TEXT NOT NULL,
  subject_id INTEGER NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(subject_id),
  FOREIGN KEY (subject_lock_owner_researcher_id) REFERENCES researchers(researcher_id)
);

CREATE TABLE response_locks (
  response_lock_id INTEGER PRIMARY KEY,
  response_lock_hash TEXT NOT NULL UNIQUE,
  response_lock_owner_researcher_id INTEGER NOT NULL,
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  timestamp TEXT NOT NULL,
  FOREIGN KEY (study_id, cue_id, response_id) REFERENCES study_cue_responses(study_id, cue_id, response_id),
  FOREIGN KEY (response_lock_owner_researcher_id) REFERENCES researchers(researcher_id)
);

CREATE TABLE response_map_confidence (
  response_map_confidence_id INTEGER PRIMARY KEY NOT NULL,
  response_map_confidence_code TEXT NOT NULL UNIQUE,
  response_map_confidence_desc TEXT NOT NULL UNIQUE
);

INSERT INTO response_map_confidence (
  response_map_confidence_id,
  response_map_confidence_code,
  response_map_confidence_desc
)
VALUES
  (1, 'not reviewed',         'The response has not been manually reviewed in the context of this study and cue.'),
  (2, 'unconfident',          'The researcher is not confident about the mapping or revision.'),
  (3, 'reasonably confident', 'The researcher is reasonably confident the mapping or revision is good, but believes a second pass would be worthwile.'),
  (4, 'confident',            'The researcher is confident the mapping or revision is good enough to work with.')
;

CREATE TABLE response_map_type (
  response_map_type_id INTEGER PRIMARY KEY NOT NULL,
  response_map_type_code TEXT NOT NULL UNIQUE,
  response_map_type_desc TEXT NOT NULL UNIQUE
);

INSERT INTO response_map_type (
  response_map_type_id,
  response_map_type_code,
  response_map_type_desc
)
VALUES
  (1, 'not mapped', 'The response has not been manually or automatically mapped'),
  (2, 'exact',      'The response matches an entry in the database exactly.'),
  (3, 'inherited',  'The response has been mapped to a database entry in a prior study, and that mapping has been inherited.'),
  (4, 'manual',     'The response received a new manual mapping to a database entry.')
;

-- Note that dates are stored NUMERIC, and can be encoded into dates in R with
-- `as.POSIXct()`
CREATE TABLE response_map (
  response_map_id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  subtlex_id INTEGER,
  subtlex_response_map_confidence_id INTEGER NOT NULL,
  subtlex_response_map_type_id INTEGER NOT NULL,
  kuperman_id INTEGER,
  kuperman_response_map_confidence_id INTEGER NOT NULL,
  kuperman_response_map_type_id INTEGER NOT NULL,
  response_revision TEXT,
  response_revision_confidence_id INTEGER NOT NULL,
  response_map_owner_researcher_id INTEGER NOT NULL,
  response_map_time NUMERIC NOT NULL,
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (cue_id) REFERENCES cues(cue_id),
  FOREIGN KEY (response_id) REFERENCES responses(response_id),
  FOREIGN KEY (response_map_owner_researcher_id) REFERENCES researchers(researcher_id),
  FOREIGN KEY (subtlex_id) REFERENCES subtlex(subtlex_id),
  FOREIGN KEY (kuperman_id) REFERENCES kuperman(kuperman_id),
  FOREIGN KEY (kuperman_response_map_confidence_id) REFERENCES response_map_confidence(response_map_confidence_id),
  FOREIGN KEY (kuperman_response_map_type_id) REFERENCES response_map_type(response_map_type_id),
  FOREIGN KEY (subtlex_response_map_confidence_id) REFERENCES response_map_confidence(response_map_confidence_id),
  FOREIGN KEY (subtlex_response_map_type_id) REFERENCES response_map_type(response_map_type_id),
  FOREIGN KEY (response_revision_confidence_id) REFERENCES response_map_confidence(response_map_confidence_id)
);


