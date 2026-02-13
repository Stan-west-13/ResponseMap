PRAGMA foreign_keys = ON;

CREATE TABLE conditions (
  condition_id INTEGER PRIMARY KEY NOT NULL,
  condition_code TEXT NOT NULL,
  condition_desc TEXT NOT NULL
)

-- TODO: map cues to information in other databases
CREATE TABLE cues (
  cue_id INTEGER PRIMARY KEY NOT NULL,
  cue TEXT NOT NULL
)

CREATE TABLE subjects (
  subject_id INTEGER PRIMARY KEY NOT NULL,
  subject_code TEXT NOT NULL,
  age_years INTEGER,
  study_id INTEGER NOT NULL,
  condition_id INTEGER NOT NULL,
  quality_id INTEGER,
  education_level_id INTEGER,
  income_level_id INTEGER,
  gender_identity_id INTEGER,
  racial_identity_id INTEGER,
  ethnic_identity_id INTEGER,
  autism_identity INTEGER,
  is_parent BOOLEAN,
  ToddlerInteractions TEXT,
  FreqToddlerInteract TEXT,
  LIST_ID INTEGER NOT NULL,
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (condition_id) REFERENCES conditions(id),
  FOREIGN KEY (quality_id) REFERENCES quality(id),
);

CREATE TABLE toddler_interactions(

);

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

CREATE TABLE toddler_interactions (
  toddler_interaction_id INTEGER PRIMARY KEY NOT NULL,
  subject_id INTEGER NOT NULL,
  toddler_interaction_question_id INTEGER NOT NULL,
  toddler_interaction_valid_response_id INTEGER NOT NULL,
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
  (1, "is_parent_of_toddler", "Are you currently the parent or guardian of a toddler?"),
  (2, "was_parent_of_toddler", "Are you a parent or guardian who raised a toddler?"),
  (3, "not_parent_interaction", "Do you ever interact with toddlers? Perhaps as an aunt, uncle, grandparent, teacher, therapist, coach, or as a friend of parents with a toddler."),
  (4, "not_parent_interaction_frequency", "How often do you interact with toddlers?")
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
  ( 1, "no",      0, 1),
  ( 2, "yes",     1, 1),
  ( 3, "no",      0, 2),
  ( 4, "yes",     1, 2),
  ( 5, "no",      0, 3),
  ( 6, "yes",     1, 3),
  ( 7, "rarely",  0, 4),
  ( 8, "monthly", 1, 4),
  ( 9, "weekly",  2, 4),
  (10, "daily",   3, 4)
;

CREATE TABLE assert (
  assert_id INTEGER PRIMARY KEY NOT NULL,
  subject_id INTEGER NOT NULL,
  assert_question_id INTEGER NOT NULL,
  assert_valid_response_id INTEGER NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(subject_id),
  FOREIGN KEY (assert_question_id) REFERENCES assert_questions(assert_question_id),
  FOREIGN KEY (assert_valid_response_id) REFERENCES assert_valid_responses(assert_valid_response_id)
);
  

CREATE TABLE assert_questions (
  assert_question_id INTEGER PRIMARY KEY NOT NULL,
  assert_question_code TEXT,
  assert_question_text TEXT
);

INSERT INTO assert_questions (id, code, question)
VALUES
  (1, "S1", "Do you find it difficult to socialize with, or to get in touch with people, especially people your own age?"),
  (2, "S2", "Do you prefer to be alone rather than being together with other people?"),
  (3, "S3", "Do you have difficulties perceiving social cues?"),
  (4, "S4", "Do other people tell you that your behavior or your emotional responses are inappropriate or hurtful?"),
  (5, "R1", "Do you have a strong interest or hobby that absorbs so much of your time that it hampers other activities?"),
  (6, "R2", "Do you or do other people feel that you have very set routines or that you are very immersed in your own interests?"),
  (7, "R3", "Do you or do other people feel that you impose your routines or interests on others?")
;

CREATE TABLE assert_valid_responses (
  assert_valid_response_id INTEGER PRIMARY KEY NOT NULL,
  assert_valid_response_text TEXT NOT NULL,
  assert_valid_response_score INTEGER NOT NULL,
  assert_question_id INTEGER,
  FOREIGN KEY (question_id) REFERENCES assert_questions(assert_question_id),
);

INSERT INTO assert_valid_responses (
  assert_valid_response_id,
  assert_valid_response_text,
  assert_valid_response_score,
  assert_question_id
)
VALUES
  ( 1, "not true",       0, 1),
  ( 2, "somewhat true",  1, 1),
  ( 3, "certainly true", 2, 1),
  ( 4, "not true",       0, 2),
  ( 5, "somewhat true",  1, 2),
  ( 6, "certainly true", 2, 2),
  ( 7, "not true",       0, 3),
  ( 8, "somewhat true",  1, 3),
  ( 9, "certainly true", 2, 3),
  (10, "not true",       0, 4),
  (11, "somewhat true",  1, 4),
  (12, "certainly true", 2, 4),
  (13, "not true",       0, 5),
  (14, "somewhat true",  1, 5),
  (15, "certainly true", 2, 5),
  (16, "not true",       0, 6),
  (17, "somewhat true",  1, 6),
  (18, "certainly true", 2, 6),
  (19, "not true",       0, 7),
  (20, "somewhat true",  1, 7),
  (21, "certainly true", 2, 7)
;


CREATE TABLE autism_identity_valid_responses (
  id INTEGER PRIMARY KEY NOT NULL,
  identify_as_autistic TEXT NOT NULL UNIQUE,
  response TEXT NOT NULL UNIQUE
);


INSERT INTO autism_identity_valid_responses (id, identify_as_autistic, diagnosed, response)
  (1, "yes", "yes", "Yes. I have received a diagnosis of Autism Spectrum Disorder."),
  (2, "yes", "no", "Yes. I have not been formally diagnosed with an Autism Spectrum Disorder, but I identify as Autistic."),
  (3, "no", "yes", "I have been diagnosed with an Autism Spectrum Disorder, but I do not identify as Autistic."),
  (4, "maybe", "yes", "It's complicated. I have been diagnosed with an Autism Spectrum Disorder, but I am not sure if I agree."),
  (5, "no", "no", "No. I have not been diagnosed with an Autism Spectrum Disorder and I do not identify as Autistic.")
;


CREATE TABLE conditions (
  condition_id INTEGER PRIMARY KEY NOT NULL,
  label TEXT NOT NULL,
  description TEXT NOT NULL
  
);

CREATE TABLE quality (
  id INTEGER PRIMARY KEY NOT NULL,
  quality TEXT,
  description TEXT
);

INSERT INTO quality (id, quality, description)
VALUES
  (1, "ideal", "Completed the task conscientiously"),
  (2, "fine",  "Completed the task mostly well, but with some careless or low effort behavior"),
  (3, "careless",  "Completed the task, but with evident lack of care throughout"),
  (4, "bad", "Completed the task, but responses are garbage"),
  (5, "incomplete", "The task is incomplete")
;


CREATE TABLE studies (
  study_id INTEGER PRIMARY KEY NOT NULL,
  label TEXT,
  abbreviation TEXT,
  description TEXT
);

CREATE TABLE response_behaviors (
  id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  response_order INTEGER NOT NULL,
  cue_order INTEGER NOT NULL,
  subject_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(subject_id),
  FOREIGN KEY (cue_id) REFERENCES cues(id),
  FOREIGN KEY (response_id) REFERENCES responses(id),
  FOREIGN KEY (study_id) REFERENCES study(id)
);


CREATE TABLE responses (
  id INTEGER PRIMARY KEY NOT NULL,
  response TEXT NOT NULL
);

CREATE TABLE studies_cues (
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  PRIMARY KEY (study_id, cue_id),
  FOREIGN KEY (study_id) REFERENCES study(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id)
);

CREATE TABLE cues (
  id INTEGER PRIMARY KEY NOT NULL,
  cue TEXT NOT NULL
);

CREATE TABLE studies_cues_responses (
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  PRIMARY KEY (study_id, cue_id, response_id),
  FOREIGN KEY (study_id) REFERENCES study(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id),
  FOREIGN KEY (response_id) REFERENCES responses(id)
);

# Note that dates are stored NUMERIC, and can be encoded into dates in R with
# `as.POSIXct()`
CREATE TABLE response_map (
  response_map_id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  cue_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  researcher_id INTEGER,
  subtlex_id INTEGER,
  kuperman_id INTEGER,
  revision TEXT,
  timestamp NUMERIC,
  FOREIGN KEY (study_id) REFERENCES study(id),
  FOREIGN KEY (cue_id) REFERENCES cues(id),
  FOREIGN KEY (response_id) REFERENCES responses(id),
  FOREIGN KEY (researcher_id) REFERENCES researchers(id),
  FOREIGN KEY (subtlex_id) REFERENCES subtlex(id),
  FOREIGN KEY (kuperman_id) REFERENCES kuperman(id),
);


CREATE TABLE researchers (
  id INTEGER PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);


INSERT INTO researchers (id, first_name, last_name, email)
VALUES
  (1, "Stan",     "West",    "swest19@lsu.edu"),
  (2, "Chris",    "Cox",     "chriscox@lsu.edu"),
  (3, "Meghan",  "Garcelon", "mgarce3@lsu.edu"),
  (4, "Angelina", "Chauvin",  "achau22@lsu.edu"),
  (5, "Hannah", "Pedigo", "hpedig2@lsu.edu"),
  (6, "Sophie", "Vidrine", "svidri8@lsu.edu"),
  (7, "Francesca", "Thomassee", "fthom22@lsu.edu"),
  (8, "Erin",     "Jines",    "ejines1@lsu.edu"),
  (9, "Trinity",    "Phipps",     "tphipp4@lsu.edu"),
  (10, "Daniela",  "Ucles", "ducles1@lsu.edu"),
  (11, "Marissa", "Goldthorp",  "mgoldt1@lsu.edu"),
  (12, "Camila", "Astete", "castet2@lsu.edu"),
  (13, "Layla", "Canaday", "lcanad2@lsu.edu")
;


CREATE TABLE subject_locks (
  id INTEGER PRIMARY KEY,
  hash TEXT NOT NULL UNIQUE,
  subject_id INTEGER NOT NULL,
  researcher_id INTEGER NOT NULL,
  timestamp TEXT NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(subject_id),
  FOREIGN KEY (researcher_id) REFERENCES researchers(id)
);

CREATE TABLE response_locks (
  id INTEGER PRIMARY KEY,
  hash TEXT NOT NULL UNIQUE,
  cue_response_id INTEGER NOT NULL,
  researcher_id INTEGER NOT NULL,
  timestamp TEXT NOT NULL,
  FOREIGN KEY (cue_response_id) REFERENCES cues_responses(id),
  FOREIGN KEY (researcher_id) REFERENCES researchers(id)
);

CREATE TABLE kuperman (
  id INTEGER PRIMARY KEY NOT NULL,
  word TEXT NOT NULL,
  aoa REAL NOT NULL
);


CREATE TABLE subtlex (
  id INTEGER PRIMARY KEY NOT NULL,
  word TEXT NOT NULL,
  Lg10WF REAL NOT NULL,
  Lg10CD REAL NOT NULL
);





