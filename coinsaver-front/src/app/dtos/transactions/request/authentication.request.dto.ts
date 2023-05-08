export class AuthenticationRequestDto {
  constructor(
    public email?: string,
    public password?: string,
  ) {}
}
