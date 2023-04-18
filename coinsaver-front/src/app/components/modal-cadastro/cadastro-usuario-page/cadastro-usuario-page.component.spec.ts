import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CadastroUsuarioPageComponent } from './cadastro-usuario-page.component';

describe('CadastroPageComponent', () => {
  let component: CadastroUsuarioPageComponent;
  let fixture: ComponentFixture<CadastroUsuarioPageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CadastroUsuarioPageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CadastroUsuarioPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
