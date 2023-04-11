import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ModalCadastroNovaTransacaoComponent } from './modal-cadastro-nova-transacao.component';

describe('ModalCadastroNovaTransacaoComponent', () => {
  let component: ModalCadastroNovaTransacaoComponent;
  let fixture: ComponentFixture<ModalCadastroNovaTransacaoComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ModalCadastroNovaTransacaoComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ModalCadastroNovaTransacaoComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
