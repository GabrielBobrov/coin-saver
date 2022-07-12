using CoinSaver.Domain.Validators;

namespace CoinSaver.Domain.Entities
{
    public class User : Base
    {
        /// <summary>
        /// Nome do usuário
        /// </summary>
        public string Name { get; set; }

        /// <summary>
        /// E-mail do usuário
        /// </summary>
        public string Email { get; set; }

        /// <summary>
        /// Data de criação.
        /// </summary>
        public DateTime CreatedAt { get; set; }

        /// <summary>
        /// Bloqueado em.
        /// </summary>
        public DateTime? BlockedAt { get; set; }

        // EF Relations

        public List<BankAccount> BankAccounts { get; set; }

        public bool Validate()
            => base.Validate(new UserValidator(), this);
    }
}
