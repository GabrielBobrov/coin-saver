package com.coinsaver.domain.entities;

import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
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
import jakarta.persistence.OneToMany;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

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
    private LocalDate payDay;

    @Column
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private StatusType status;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private TransactionCategoryType category;

    @Column(nullable = false)
    private TransactionType transactionType;

    @Column
    private Boolean fixedExpense;

    @Column
    private Integer repeat_;

    @ManyToOne
    @JoinColumn(name = "client_id")
    private Client client;

    @OneToMany(mappedBy = "transaction", cascade = CascadeType.ALL)
    private List<FixTransaction> fixTransactions;

    @OneToMany(mappedBy = "transaction", cascade = CascadeType.ALL)
    private List<InstallmentTransaction> installmentTransactions;

    @ManyToOne
    @JoinColumn(name = "division_id")
    private Division division;

    public void payTransaction() {
        this.setStatus(StatusType.PAID);
    }

    public void receiveTransaction() {
        this.setStatus(StatusType.RECEIVED);
    }
}
