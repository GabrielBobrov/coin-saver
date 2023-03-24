import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-login-page',
  templateUrl: './login-page.component.html',
  styleUrls: ['./login-page.component.css']
})
export class LoginPageComponent {

  constructor(
    public router: Router,
  ) {}

  abrirCadastroPage() {
    this.router.navigateByUrl('cadastro-page', {
      state: {
        data: {},
      },
    });
  }

}
