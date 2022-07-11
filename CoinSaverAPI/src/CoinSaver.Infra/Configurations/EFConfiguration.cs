using Microsoft.EntityFrameworkCore;
using CoinSaver.Core.Enums;
using CoinSaver.Infra.Mappings;

namespace CoinSaver.Infra.Configuration
{
    public static class EFConfigurations
    {
        public static ModelBuilder AddMappings(this ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new OperatorMapping());

            return modelBuilder;
        }

        public static ModelBuilder AddPostgresEnums(this ModelBuilder modelBuilder)
        {
            modelBuilder.HasPostgresEnum<StatusType>();
            modelBuilder.HasPostgresEnum<TransactionCategoryType>();
            return modelBuilder;
        }
    }
}
