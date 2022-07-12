using MediatR;
using System;

namespace CoinSaver.Core.Communication.Messages.Notifications
{
    public abstract class Notification : INotification
    {
        public string Hash { get; private set; }

        public Notification()
        {
            Hash = Guid.NewGuid().ToString();
        }
    }
}