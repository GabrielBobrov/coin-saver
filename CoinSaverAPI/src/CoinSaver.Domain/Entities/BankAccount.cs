namespace CoinSaver.Domain.Entities
{
    public class BankAccount : Base
    {
        /// <summary>
        /// Id do usuário.
        /// </summary>
        public long UserId { get; set; }

        /// <summary>
        /// Nome do banco.
        /// </summary>
        public string BankName { get; set; }

        /// <summary>
        /// Agência bancária.
        /// </summary>
        public string Office { get; set; }

        /// <summary>
        /// Número da conta sem o dígito.
        /// </summary>
        public string AccountNumber { get; set; }

        /// <summary>
        /// Dígito da conta.
        /// </summary>
        public string AccountDigit { get; set; }

        // EF Relations

        public User User { get; set; }
    }
}
