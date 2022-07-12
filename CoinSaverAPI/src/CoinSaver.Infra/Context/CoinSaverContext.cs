using System;
using CoinSaver.Domain.Entities;
using CoinSaver.Infra.Mappings;
using Microsoft.EntityFrameworkCore;
using CoinSaver.Infra.Configuration;
using Npgsql;

namespace CoinSaver.Infra.Context
{
    public class CoinSaverContext : DbContext
    {

        static CoinSaverContext()
        {
            NpgsqlConnection.GlobalTypeMapper.AddGlobalTypeMappers();
        }
        public CoinSaverContext()
        { }

        public CoinSaverContext(DbContextOptions<CoinSaverContext> options) : base(options)
        { }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuider)
        {
            optionsBuider.UseNpgsql("Server=127.0.0.1;Port=5432;Database=CoinSaverDB;User Id=postgres;Password=gabriel123;Timeout=15;");
        }

        public virtual DbSet<BankAccount> Users { get; set; }

        protected override void OnModelCreating(ModelBuilder builder)
        {
            builder.AddPostgresEnums();
            builder.AddMappings();

            base.OnModelCreating(builder);
        }
    }
}