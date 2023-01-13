package com.coinsaver.domain.entities;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import org.hibernate.annotations.CreationTimestamp;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@Entity
public class Transaction {
	
	@EqualsAndHashCode.Include
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	
	@Column
	private BigDecimal amount;
	
	@CreationTimestamp
	private LocalDateTime createdAt;
	
	@Column(nullable = false)
	private LocalDateTime payDay;
	
	@Column
	private String description;
	
	@Enumerated(EnumType.ORDINAL)
	@Column(nullable = false)
	private StatusType status;
	
	@Enumerated(EnumType.ORDINAL)
	@Column(nullable = false)
	private TransactionCategoryType category;
	
	@Column
	private Boolean fixedExpense;
	
	@Column
	private Integer repeat;
	
	@ManyToOne
	@JoinColumn(name = "client_id")
	private Client client;

}
