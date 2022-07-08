using System;
using System.ComponentModel.DataAnnotations;
using CoinSaver.Core.Enums;
using CoinSaver.Domain.Validators;

namespace CoinSaver.Domain.Entities
{
	public class Transaction : Base
	{
        public Transaction(decimal amount, DateTime date, string description, StatusType status, TransactionCategoryType category)
        {
            Amount = amount;
            Date = date;
            Description = description;
            Status = status;
            Category = category;
            Validate();
        }

        public decimal Amount { get; private set; }

		public DateTime Date { get; private set; }

        public string Description { get; private set; }

        public StatusType Status { get; private set; }

        public TransactionCategoryType Category { get; private set; }

        public bool Validate()
            => base.Validate(new TransactionValidator(), this);
    }
}

