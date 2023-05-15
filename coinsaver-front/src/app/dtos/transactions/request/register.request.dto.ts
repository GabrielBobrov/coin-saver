export class RegisterRequestDto {
  constructor(
    public id?: number,
    public name?: string,
    public email?: string,
    public contraEmail?: string,
    public password?: string,
    public contraPassword?: string,
  ) {}
}
