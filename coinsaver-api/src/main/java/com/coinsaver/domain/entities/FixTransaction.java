package com.coinsaver.domain.entities;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import jakarta.persistence.CascadeType;
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
import org.hibernate.annotations.CreationTimestamp;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@Entity
public class FixTransaction extends TransactionBase {

    @EqualsAndHashCode.Include
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private BigDecimal amount;

    @CreationTimestamp
    private LocalDateTime createdAt;

    @Column(nullable = false)
    private LocalDate payDay;

    @Column
    private String description;

    @Enumerated(EnumType.ORDINAL)
    @Column(nullable = false)
    private StatusType status;

    @Enumerated(EnumType.ORDINAL)
    @Column(nullable = false)
    private TransactionCategoryType category;

    @Column(nullable = false)
    private Boolean edited;

    @ManyToOne
    @JoinColumn(name = "transaction_id")
    private Transaction transaction;

    public void payTransaction() {
        this.setStatus(StatusType.PAID);
    }
}
