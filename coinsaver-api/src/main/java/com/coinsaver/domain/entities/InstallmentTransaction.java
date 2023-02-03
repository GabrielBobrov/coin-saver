package com.coinsaver.domain.entities;

import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
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
public class InstallmentTransaction extends TransactionBase {

    @EqualsAndHashCode.Include
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
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

    @ManyToOne
    @JoinColumn(name = "transaction_id")
    private Transaction transaction;
}
