package com.coinsaver.core.enums;

/**
 * Categoria da transação
 * @author Gabriel
 *
 */
public enum TransactionCategoryType {
	EXPENSE("Despesa"),
	INCOME("Entrada");

	private final String name;

	TransactionCategoryType(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
