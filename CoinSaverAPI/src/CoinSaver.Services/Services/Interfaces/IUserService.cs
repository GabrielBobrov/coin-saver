using System.Threading.Tasks;
using System.Collections.Generic;
using CoinSaver.Services.Dtos;
using CoinSaver.Core.Structs;
using CoinSaver.Core.Enums;

namespace CoinSaver.Services.Interfaces
{
    public interface IUserService
    {
        Task<Optional<UserDto>> CreateAsync(UserDto operatorDto);
    }
}