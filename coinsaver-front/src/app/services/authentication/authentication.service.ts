import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs';
import { AuthenticationRequestDto } from 'src/app/dtos/transactions/request/authentication.request.dto';
import { RegisterRequestDto } from 'src/app/dtos/transactions/request/register.request.dto';
import { environment } from 'src/app/environments/environments';

@Injectable({
  providedIn: 'root'
})
export class AuthenticationService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  baseUrl = environment.api.hostBackend;
  authenticationControllerUrl = environment.api.authenticationControllerBackend;

  token?: string;

  recebeToken(token: any) {
    this.token = JSON.stringify(token.token).replaceAll('"', '');
  }

  register(registerRequestDto: RegisterRequestDto) {
    const url = this.baseUrl + this.authenticationControllerUrl + environment.api.authenticationBackendEndpoints.register;
    const body = JSON.stringify(registerRequestDto);
    const headers = { 'Content-Type': 'application/json' };

    return this.httpClient.post(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }

  authenticate(authenticationRequestDto: AuthenticationRequestDto) {
    const url = this.baseUrl + this.authenticationControllerUrl + environment.api.authenticationBackendEndpoints.authenticate;
    const body = JSON.stringify(authenticationRequestDto);
    const headers = { 'Content-Type': 'application/json' };

    return this.httpClient.post(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }
}
