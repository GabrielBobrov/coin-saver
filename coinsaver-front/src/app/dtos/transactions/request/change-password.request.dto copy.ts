export class ChangePasswordRequestDto {
  constructor(
    public oldPassword?: string,
    public newPassword?: string,
    public newPasswordVerify?: string,
  ) {}
}
