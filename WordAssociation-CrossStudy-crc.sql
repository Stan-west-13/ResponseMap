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
  id INTEGER PRIMARY KEY NOT NULL,
  study_id INTEGER NOT NULL,
  subject_code TEXT NOT NULL,
  quality_id INTEGER,
  Age INTEGER,
  EduLevel TEXT,
  Gender TEXT,
  IncomeLevel TEXT,
  Race TEXT,
  Ethnicity TEXT,
  S1 TEXT,
  S2 TEXT,
  S3 TEXT,
  R1 TEXT,
  R2 TEXT,
  R3 TEXT,
  ASD_identity TEXT,
  Parent TEXT,
  ToddlerInteractions TEXT,
  FreqToddlerInteract TEXT,
  COND_ID INTEGER NOT NULL,
  LIST_ID INTEGER NOT NULL,
  condition TEXT,
  FOREIGN KEY (quality_id) REFERENCES quality(id),
  FOREIGN KEY (condition_id) REFERENCES conditions(id)
);

CREATE TABLE conditions (
  id INTEGER PRIMARY KEY NOT NULL,
  label TEXT NOT NULL,
  description TEXT NOT NULL
  
)

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
  id INTEGER PRIMARY KEY NOT NULL,
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
  FOREIGN KEY (subject_id) REFERENCES subjects(id),
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
  id INTEGER PRIMARY KEY NOT NULL,
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
  FOREIGN KEY (subject_id) REFERENCES subjects(id),
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





