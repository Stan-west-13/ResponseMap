PRAGMA foreign_keys = ON;

CREATE TABLE conditions (
  id INTEGER PRIMARY KEY NOT NULL,
  label TEXT NOT NULL,
  description TEXT NOT NULL
)

CREATE TABLE cues (
  id INTEGER PRIMARY KEY NOT NULL,
  cue TEXT NOT NULL
)

CREATE TABLE subjects (
  subject_id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  subject_code TEXT NOT NULL,
  quality_id INTEGER,
  Age INTEGER,
  EduLevel TEXT,
  Gender TEXT,
  IncomeLevel TEXT,
  Race TEXT,
  Ethnicity TEXT,
  autism_identity INTEGER,
  Parent TEXT,
  ToddlerInteractions TEXT,
  FreqToddlerInteract TEXT,
  condition_id INTEGER NOT NULL,
  LIST_ID INTEGER NOT NULL,
  FOREIGN KEY (quality_id) REFERENCES quality(id),
  FOREIGN KEY (condition_id) REFERENCES conditions(id)
);

CREATE TABLE education_levels (
  
);

CREATE TABLE assert (
  assert_id INTEGER PRIMARY KEY NOT NULL,
  subject_id INTEGER NOT NULL,
  question_id INTEGER NOT NULL,
  response_id INTEGER NOT NULL,
  FOREIGN KEY (subject_id) REFERENCES subjects(subject_id),
  FOREIGN KEY (question_id) REFERENCES ASSERT_questions(id),
  FOREIGN KEY (response_id) REFERENCES ASSERT_valid_responses(id)
);
  

CREATE TABLE assert_questions (
  id INTEGER PRIMARY KEY NOT NULL,
  code TEXT,
  question TEXT
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

CREATE TABLE ASSERT_valid_responses (
  id INTEGER PRIMARY KEY NOT NULL,
  question_id INTEGER
  response TEXT NOT NULL,
  score INTEGER NOT NULL,
  FOREIGN KEY (question_id) REFERENCES ASSERT_questions(id),
);

INSERT INTO ASSERT_valid_responses (id, question_id, response, score)
VALUES
  ( 1, 1, "not true", 0),
  ( 2, 1, "somewhat true", 1),
  ( 3, 1, "certainly true", 2),
  ( 4, 2, "not true", 0),
  ( 5, 2, "somewhat true", 1),
  ( 6, 2, "certainly true", 2),
  ( 7, 3, "not true", 0),
  ( 8, 3, "somewhat true", 1),
  ( 9, 3, "certainly true", 2),
  (10, 4, "not true", 0),
  (11, 4, "somewhat true", 1),
  (12, 4, "certainly true", 2),
  (13, 5, "not true", 0),
  (14, 5, "somewhat true", 1),
  (15, 5, "certainly true", 2),
  (16, 6, "not true", 0),
  (17, 6, "somewhat true", 1),
  (18, 6, "certainly true", 2),
  (19, 7, "not true", 0),
  (20, 7, "somewhat true", 1),
  (21, 7, "certainly true", 2)
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





