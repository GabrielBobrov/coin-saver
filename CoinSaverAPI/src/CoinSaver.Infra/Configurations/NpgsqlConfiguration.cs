using Npgsql.TypeMapping;
using CoinSaver.Core.Enums;

namespace CoinSaver.Infra.Configuration
{
    public static class NpgsqlConfiguration
    {
        public static void AddGlobalTypeMappers(this INpgsqlTypeMapper mapper)
        {
            mapper.MapEnum<StatusType>();
            mapper.MapEnum<TransactionCategoryType>();
        }
    }
}
