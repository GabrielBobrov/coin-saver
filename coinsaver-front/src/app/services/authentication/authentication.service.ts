import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs';
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

  register(registerRequestDto: RegisterRequestDto) {
    const url = this.baseUrl + this.authenticationControllerUrl + environment.api.authenticationBackendEndpoints.register;
    const body = JSON.stringify(registerRequestDto);
    const headers = { 'Content-Type': 'application/json' };

    console.log(url)

    return this.httpClient.post(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }
}
