using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Threading.Tasks;
using AutoMapper;
using CoinSaver.Core.Communication.Mediator.Interfaces;
using CoinSaver.Core.Communication.Messages.Notifications;
using CoinSaver.Core.Enums;
using CoinSaver.Core.Structs;
using CoinSaver.Core.Validations.Messages;
using CoinSaver.Domain.Entities;
using CoinSaver.Infra.Repositories.Interfaces;
using CoinSaver.Services.Dtos;
using CoinSaver.Services.Interfaces;

namespace CoinSaver.Services.Services
{
    public class UserService : IUserService
    {
        private readonly IMapper _mapper;
        private readonly IUserRepository _operatorRepository;
        private readonly IMediatorHandler _mediator;

        public UserService(
            IMapper mapper,
            IUserRepository operatorRepository,
            IMediatorHandler mediator)
        {
            _mapper = mapper;
            _operatorRepository = operatorRepository;
            _mediator = mediator;
        }

        public async Task<Optional<UserDto>> CreateAsync(UserDto operatorDto)
        {
            Expression<Func<User, bool>> filter = op
                => op.Name.ToLower() == operatorDto.Name.ToLower();

            var userExists = await _operatorRepository.GetAsync(filter);

            if (userExists != null)
            {
                await _mediator.PublishDomainNotificationAsync(new DomainNotification(
                    ErrorMessages.UserAlreadyExists,
                    DomainNotificationType.UserAlreadyExists));

                return new Optional<UserDto>();


            }

            var user = _mapper.Map<User>(operatorDto);
            user.Validate();

            if (!user.IsValid)
            {
                await _mediator.PublishDomainNotificationAsync(new DomainNotification(
                   ErrorMessages.UserInvalid(user.ErrorsToString()),
                   DomainNotificationType.UserInvalid));

                return new Optional<UserDto>();
            }

            var userCreated = await _operatorRepository.CreateAsync(user);

            return _mapper.Map<UserDto>(userCreated);
        }
    }
}