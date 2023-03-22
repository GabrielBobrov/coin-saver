INSERT INTO CLIENT(NAME, EMAIL, CREATED_AT) VALUES ('cliente 1', 'ciente@hotmail.com', CURRENT_TIMESTAMP());

INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (100, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 1, 0, 1, 0, null, 1);
INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (150, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 2, 1, 2, 1, null, 1);
INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (200, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 3, 1, 0, 0, 3,    1);
INSERT INTO TRANSACTION(AMOUNT, CREATED_AT, DESCRIPTION,  PAY_DAY, STATUS, CATEGORY, TRANSACTION_TYPE, FIXED_EXPENSE, REPEAT, CLIENT_ID) VALUES (100, CURRENT_TIMESTAMP(), 'DESCRIPTION', CURRENT_TIMESTAMP() , 1, 0, 0, 0, 2,    1);
create table oauth_code (
                            code varchar(256),
                            authentication blob
);

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
CREATE TABLE oauth2_registered_client (
                                          id varchar(100) NOT NULL,
                                          client_id varchar(100) NOT NULL,
                                          client_id_issued_at timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL,
                                          client_secret varchar(200) DEFAULT NULL,
                                          client_secret_expires_at timestamp DEFAULT NULL,
                                          client_name varchar(200) NOT NULL,
                                          client_authentication_methods varchar(1000) NOT NULL,
                                          authorization_grant_types varchar(1000) NOT NULL,
                                          redirect_uris varchar(1000) DEFAULT NULL,
                                          scopes varchar(1000) NOT NULL,
                                          client_settings varchar(2000) NOT NULL,
                                          token_settings varchar(2000) NOT NULL,
                                          PRIMARY KEY (id)
);

CREATE TABLE oauth2_authorization_consent (
                                              registered_client_id varchar(100) NOT NULL,
                                              principal_name varchar(200) NOT NULL,
                                              authorities varchar(1000) NOT NULL,
                                              PRIMARY KEY (registered_client_id, principal_name)
);


INSERT INTO oauth2_registered_client
(id, client_id, client_id_issued_at, client_secret, client_secret_expires_at, client_name, client_authentication_methods, authorization_grant_types, redirect_uris, scopes, client_settings, token_settings)
VALUES('1', 'algafood-backend', '2022-11-29 18:58:12', '$2a$10$trk401po.Wx9JXXMs2xCFeB.eXU7qENFquETcr04a0hDJxGV3ge0.', NULL, 'AlgaFood Backend', 'client_secret_basic', 'client_credentials', '', 'READ', '{"@class":"java.util.Collections$UnmodifiableMap","settings.client.require-proof-key":false,"settings.client.require-authorization-consent":false}', '{"@class":"java.util.Collections$UnmodifiableMap","settings.token.reuse-refresh-tokens":true,"settings.token.id-token-signature-algorithm":["org.springframework.security.oauth2.jose.jws.SignatureAlgorithm","RS256"],"settings.token.access-token-time-to-live":["java.time.Duration",1800.000000000],"settings.token.access-token-format":{"@class":"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat","value":"self-contained"},"settings.token.refresh-token-time-to-live":["java.time.Duration",3600.000000000],"settings.token.authorization-code-time-to-live":["java.time.Duration",300.000000000]}');

INSERT INTO oauth2_registered_client
(id, client_id, client_id_issued_at, client_secret, client_secret_expires_at, client_name, client_authentication_methods, authorization_grant_types, redirect_uris, scopes, client_settings, token_settings)
VALUES('2', 'algafood-web', '2022-11-29 18:58:12', '$2a$10$/Lx1cVKanXiCkpYtdA369OZ78x8aHwx51RTxC.4pqEiuZRzQh0e/i', NULL, 'AlgaFood Web', 'client_secret_basic', 'refresh_token,authorization_code', 'http://127.0.0.1:8080/swagger-ui/oauth2-redirect.html,http://127.0.0.1:8080/authorized', 'READ,WRITE', '{"@class":"java.util.Collections$UnmodifiableMap","settings.client.require-proof-key":false,"settings.client.require-authorization-consent":true}', '{"@class":"java.util.Collections$UnmodifiableMap","settings.token.reuse-refresh-tokens":false,"settings.token.id-token-signature-algorithm":["org.springframework.security.oauth2.jose.jws.SignatureAlgorithm","RS256"],"settings.token.access-token-time-to-live":["java.time.Duration",900.000000000],"settings.token.access-token-format":{"@class":"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat","value":"self-contained"},"settings.token.refresh-token-time-to-live":["java.time.Duration",86400.000000000],"settings.token.authorization-code-time-to-live":["java.time.Duration",300.000000000]}');

INSERT INTO oauth2_registered_client
(id, client_id, client_id_issued_at, client_secret, client_secret_expires_at, client_name, client_authentication_methods, authorization_grant_types, redirect_uris, scopes, client_settings, token_settings)
VALUES('3', 'foodanalytics', '2022-11-29 18:58:12', '$2a$10$LQOU54Ta7zV7TxTXSk7DEeZUx/P9PwKGH5CTIOLNGWgIP29QHdq4K', NULL, 'Food Analytics', 'client_secret_basic', 'authorization_code', 'http://www.foodanalytics.local:8082', 'READ,WRITE', '{"@class":"java.util.Collections$UnmodifiableMap","settings.client.require-proof-key":false,"settings.client.require-authorization-consent":false}', '{"@class":"java.util.Collections$UnmodifiableMap","settings.token.reuse-refresh-tokens":true,"settings.token.id-token-signature-algorithm":["org.springframework.security.oauth2.jose.jws.SignatureAlgorithm","RS256"],"settings.token.access-token-time-to-live":["java.time.Duration",1800.000000000],"settings.token.access-token-format":{"@class":"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat","value":"self-contained"},"settings.token.refresh-token-time-to-live":["java.time.Duration",3600.000000000],"settings.token.authorization-code-time-to-live":["java.time.Duration",300.000000000]}');

