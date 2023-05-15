INSERT INTO client (name, email, created_at, password, password_verification, role) VALUES
('Fulano', 'fulano@example.com', CURRENT_TIMESTAMP(), '$2a$10$qvkN2RGg1mg8IOlwWWuOnunwtMfWTGBXY5usiFJa5UJ.JVqWxYYim', 'Ul9UnIc9V5hejN/PFg3yo2Un/+mk0SKT', 'USER');


INSERT INTO division(NAME, TYPE, CATEGORY, CREATED_AT) VALUES
    ('Saude', 'HEALTH', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Casa', 'HOME', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Electronic', 'ELECTRONIC', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Restaurante', 'RESTAURANT', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Academia', 'GYM', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Aluguel', 'RENT', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Supermercado', 'MARKET', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Lazer', 'LEISURE', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Combustivel', 'FUEL', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Delivery', 'DELIVERY', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('BadBet', 'BAD_BET', 'EXPENSE', CURRENT_TIMESTAMP()),
    ('Salário', 'SALARY', 'INCOME', CURRENT_TIMESTAMP()),
    ('Bônus', 'BONUS', 'INCOME', CURRENT_TIMESTAMP()),
    ('GoodBet', 'GOOD_BET', 'INCOME', CURRENT_TIMESTAMP()),
    ('Investimento', 'INVESTMENT', 'INCOME', CURRENT_TIMESTAMP());

