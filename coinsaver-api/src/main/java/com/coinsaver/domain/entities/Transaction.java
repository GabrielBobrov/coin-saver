package com.coinsaver.domain.entities;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import org.hibernate.annotations.CreationTimestamp;
import org.springframework.data.annotation.Id;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
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
	
	@Column(nullable = false)
	private BigDecimal amount;
	
	@CreationTimestamp
	private LocalDateTime createdAt;
	
	private String description;
	
	@Enumerated(EnumType.ORDINAL)
	@Column(nullable = false)
	private StatusType status;
	
	@Enumerated(EnumType.ORDINAL)
	@Column(nullable = false)
	private TransactionCategoryType category;
	
	@ManyToOne
	private User user;

}
