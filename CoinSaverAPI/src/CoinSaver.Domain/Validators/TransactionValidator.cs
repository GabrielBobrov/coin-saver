using FluentValidation;
using CoinSaver.Domain.Entities;

namespace CoinSaver.Domain.Validators
{
    public class TransactionValidator : AbstractValidator<Transaction>
    {
        public TransactionValidator()
        {
            RuleFor(x => x)
                .NotEmpty()
                .WithMessage("A entidade não pode ser vazia.")

                .NotNull()
                .WithMessage("A entidade não pode ser nula.");

            RuleFor(x => x.Description)
                .NotNull()
                .WithMessage("A descrição não pode ser nula.")

                .NotEmpty()
                .WithMessage("A descrição não pode ser vazia.")

                .MinimumLength(3)
                .WithMessage("A descrição deve ter no mínimo 3 caracteres.")

                .MaximumLength(80)
                .WithMessage("A descrição deve ter no máximo 80 caracteres.");
        }
    }
}