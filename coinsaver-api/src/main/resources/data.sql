INSERT INTO CLIENT(NAME, EMAIL, CREATED_AT, BALANCE) VALUES ('cliente 1', 'ciente@hotmail.com', CURRENT_TIMESTAMP(), 1000);

-- INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (100, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 1, 0, 1, 0, null, 1);
-- INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (150, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 2, 1, 2, 1, null, 1);
-- INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (200, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 3, 1, 0, 0, 3,    1);
-- INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (100, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 1, 0, 0, 0, 2,    1);

CREATE TABLE oauth2_authorization (
                                      id varchar(100) NOT NULL,
                                      registered_client_id varchar(100) NOT NULL,
                                      principal_name varchar(200) NOT NULL,
                                      authorization_grant_type varchar(100) NOT NULL,
                                      authorized_scopes varchar(1000) DEFAULT NULL,
                                      attributes blob DEFAULT NULL,
                                      state varchar(500) DEFAULT NULL,
                                      authorization_code_value blob DEFAULT NULL,
                                      authorization_code_issued_at timestamp DEFAULT NULL,
                                      authorization_code_expires_at timestamp DEFAULT NULL,
                                      authorization_code_metadata blob DEFAULT NULL,
                                      access_token_value blob DEFAULT NULL,
                                      access_token_issued_at timestamp DEFAULT NULL,
                                      access_token_expires_at timestamp DEFAULT NULL,
                                      access_token_metadata blob DEFAULT NULL,
                                      access_token_type varchar(100) DEFAULT NULL,
                                      access_token_scopes varchar(1000) DEFAULT NULL,
                                      oidc_id_token_value blob DEFAULT NULL,
                                      oidc_id_token_issued_at timestamp DEFAULT NULL,
                                      oidc_id_token_expires_at timestamp DEFAULT NULL,
                                      oidc_id_token_metadata blob DEFAULT NULL,
                                      refresh_token_value blob DEFAULT NULL,
                                      refresh_token_issued_at timestamp DEFAULT NULL,
                                      refresh_token_expires_at timestamp DEFAULT NULL,
                                      refresh_token_metadata blob DEFAULT NULL,
                                      PRIMARY KEY (id)
);

INSERT INTO division(NAME, TYPE, CATEGORY, CREATED_AT) VALUES ('Academia', 'GYM', 'EXPENSE', CURRENT_TIMESTAMP()),
                                                              ('Salário', 'SALARY', 'INCOME', CURRENT_TIMESTAMP()),
                                                              ('Aluguel', 'RENT', 'EXPENSE', CURRENT_TIMESTAMP()),
                                                              ('Bônus', 'BONUS', 'INCOME', CURRENT_TIMESTAMP()),
                                                              ('Supermercado', 'MARKET', 'EXPENSE', CURRENT_TIMESTAMP()),
                                                              ('Investimento', 'INVESTMENT', 'INCOME', CURRENT_TIMESTAMP());