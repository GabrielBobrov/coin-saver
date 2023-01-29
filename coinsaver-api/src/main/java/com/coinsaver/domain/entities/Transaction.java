package com.coinsaver.domain.entities;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;
import org.modelmapper.ModelMapper;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@Builder
@AllArgsConstructor
@NoArgsConstructor
@DynamicUpdate
@Entity
public class Transaction extends TransactionBase {

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
