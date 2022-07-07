using System;
using System.ComponentModel.DataAnnotations;
using CoinSaver.Domain.Validators;

namespace CoinSaver.Domain.Entities
{
	public class Transaction : Base
	{
        public Transaction(decimal amount, DateTime date, string description, string status)
        {
            Amount = amount;
            Date = date;
            Description = description;
            Status = status;
            Validate();
        }

        public decimal Amount { get; private set; }

		public DateTime Date { get; private set; }
        //create enum
        public string Description { get; private set; }
        //create enum
        public string Status { get; private set; }

        public string Category { get; private set; }

        public bool Validate()
            => base.Validate(new TransactionValidator(), this);
    }
}

