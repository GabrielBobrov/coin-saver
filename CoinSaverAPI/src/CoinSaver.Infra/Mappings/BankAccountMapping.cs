using CoinSaver.Domain.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace CoinSaver.Infra.Mappings
{
    public class BankAccountMapping : IEntityTypeConfiguration<BankAccount>
    {
        public void Configure(EntityTypeBuilder<BankAccount> builder)
        {
            builder.ToTable("BankAccount");

            builder.HasKey(x => x.Id);

            builder.Property(x => x.Id)
                .HasColumnType("BIGINT");

            builder.Property(x => x.AccountDigit)
                .IsRequired()
                .HasMaxLength(80)
                .HasColumnName("AccountDigit")
                .HasColumnType("VARCHAR(80)");
        }
    }
}