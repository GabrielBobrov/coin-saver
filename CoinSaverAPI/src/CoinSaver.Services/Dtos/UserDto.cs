using System;
namespace CoinSaver.Services.Dtos
{
	public class UserDto
	{
        public long Id { get; set; }

        public string Name { get; set; }

		public string Email { get; set; }

        public UserDto()
		{
		}
	}
}

