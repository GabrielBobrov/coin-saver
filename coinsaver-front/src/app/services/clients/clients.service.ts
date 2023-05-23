import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, map, throwError } from 'rxjs';
import { ChangePasswordRequestDto } from 'src/app/dtos/transactions/request/change-password.request.dto copy';
import { environment } from 'src/app/environments/environments';

@Injectable({
  providedIn: 'root'
})
export class ClientsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  baseUrl = environment.api.hostBackend;
  clientsControllerUrl = environment.api.clientsControllerBackend;

  token?: string;

  recebeToken(token: any) {
    this.token = JSON.stringify(token.token).replaceAll('"', '');
  }

  recoverPassword(email?: string) {
    const url = this.baseUrl + this.clientsControllerUrl + environment.api.clientsBackendEndpoints.recoverPassword
    const headers = { 'Content-Type': 'application/json' };

    return this.httpClient.post(
      (`${url}?email=${email}`), headers
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  changePassword(changePasswordnRequestDto?: ChangePasswordRequestDto) {
    const url = this.baseUrl + this.clientsControllerUrl + environment.api.clientsBackendEndpoints.changePassword
    const body = JSON.stringify(changePasswordnRequestDto);
    const headers = { 'Content-Type': 'application/json', 'Authorization': `Bearer ${(this.token)}` };

    console.log(url)
    console.log(body)
    console.log(headers)

    return this.httpClient.post(url, body, { headers }).pipe(
      map(result => {
        return result;
      })
    );
  }
}
