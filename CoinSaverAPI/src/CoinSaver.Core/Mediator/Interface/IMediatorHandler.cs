using CoinSaver.Core.Communication.Messages.Notifications;
using System.Threading.Tasks;

namespace CoinSaver.Core.Communication.Mediator.Interfaces
{
    public interface IMediatorHandler
    {
        Task PublishDomainNotificationAsync<T>(T appNotification)
            where T : DomainNotification;
    }
}