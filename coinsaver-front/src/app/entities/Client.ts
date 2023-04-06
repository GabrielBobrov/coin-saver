export class Client {
  constructor(
    public id: number,
    public name: string,
    public gender: string,
    public phone: string,
    public email: string,
    public confirmEmail: string,
    public password: string,
    public confirmPassword: string,

    public createdAt: string,

    public balance: number,
  ) {}
}
