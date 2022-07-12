using System;
using CoinSaver.Domain.Entities;
using CoinSaver.Infra.Context;
using CoinSaver.Infra.Repositories.Interfaces;

namespace CoinSaver.Infra.Repositories
{
    public class UserRepository : BaseRepository<User>, IUserRepository
    {
        private readonly CoinSaverContext _context;

        public UserRepository(CoinSaverContext context) : base(context)
        {
            _context = context;
        }
    }
}

