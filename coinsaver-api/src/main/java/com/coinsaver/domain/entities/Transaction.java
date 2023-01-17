package com.coinsaver.domain.entities;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.annotations.CreationTimestamp;
import org.modelmapper.ModelMapper;

import java.math.BigDecimal;
import java.time.LocalDateTime;

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

	public TransactionDto convertEntityToDto() {
		return new ModelMapper().map(this, TransactionDto.class);
	}

}
